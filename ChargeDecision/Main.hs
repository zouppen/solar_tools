{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Main where

import Control.Monad (when, unless)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Scientific
import qualified Data.Yaml as Y
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Newtypes (Aeson(..))
import GHC.Generics
import System.Exit

import Common.DbHelpers
import Common.ConfigHelpers
import Common.Relay
import Common.Shelly

data Config = Config
  { connString      :: ByteString -- ^PostgreSQL connection string
  , sql             :: Query      -- ^SQL to run initially
  , fullChargeAfter :: String     -- ^How often battery should reach 100%
  , respectManual   :: Maybe Bool -- ^Keep charger on if manually started (default: on)
  , relayUrl        :: String     -- ^Shelly relay URL
  , debug           :: Maybe Bool -- ^Do debug printing (default: off)
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

data State = State
  { relay            :: RelayState -- ^Current state of the relay
  , fullChargeNeeded :: Bool       -- ^Is 100% charging requested
  , soc              :: Scientific -- ^Current state-of-charge
  , profile          :: String     -- ^Free-form profile name
  , socMin           :: Scientific -- ^State-of-charge to start charging
  , socMax           :: Scientific -- ^State-of-charge to stop charging
  , allowFullCharge  :: Bool       -- ^Current profile allows 100% charge
  } deriving (Generic, Show)

instance ToJSON State where
    toEncoding = genericToEncoding defaultOptions

data Decision = Decision
  { decision    :: Bool
  , explanation :: String
  } deriving (Generic, Show)

instance ToJSON Decision where
    toEncoding = genericToEncoding defaultOptions

main :: IO ()
main = do
  config@Config{..} <- configHelper Y.decodeFileThrow
  let dbg = when (debug == Just True)
  -- Prepare relay control
  Relay{..} <- initShelly relayUrl
  -- Connect to database and prepare queries
  conn <- connectPostgreSQL connString
  execute_ conn sql
  -- Get current state of things
  state@State{..} <- collectState conn readRelay config
  let dec@Decision{..} = decide config state
  dbg $ BL.putStrLn $ "State: " <> encode state
  let doControl = case (decision, relayState relay) of
        (True, True) -> Nothing -- "On" mode needs no dead-man-switch
        (a, _)       -> Just a
  dbg $ putStrLn $ "Decision: " <> explanation <> "\nControl: " <> show doControl
  -- Store decision
  withTransaction conn $ query conn "EXECUTE decision(?)" [Aeson dec] :: IO [[()]]
  case doControl of
    Nothing -> pure ()
    Just a -> do
      out <- writeRelay a
      unless (oldState out == Just (relayState relay)) $
        die "Race condition: Relay state changed while controlling relay"

-- |Connect to database and relay and collect current state
collectState :: Connection -> RelayReader -> Config -> IO State
collectState conn readRelay Config{..} = do
  -- Read relay information
  (relay, relayRaw) <- readRelay
  -- Insert relay data in a separate transaction to make sure data
  -- collection even when control fails.
  withTransaction conn $ execute conn "EXECUTE info(?)" [relayRaw]
  -- In the second transaction, collect charger and profile data
  withTransaction conn $ do
    Just [fullChargeNeeded] <- singleQuery conn "EXECUTE full_charge_needed(?)" [fullChargeAfter]
    Just [soc] <- singleQuery conn "EXECUTE soc" ()
    Just (profile, socMin, socMax, allowFullCharge) <- singleQuery conn "EXECUTE profile" ()
    pure State{..}

-- |Do control deceision based on configuration and current state.
decide :: Config -> State -> Decision
decide Config{..} State{..} =
  case (relayState relay, fullChargeNeeded, allowFullCharge, forced) of
    (True, _    , _   , True ) -> Decision True "Manual mode"
    (True , True, True, _    ) -> Decision (soc < 100) "Target 100%"
    (False, _   , _   , _    ) -> Decision (soc < socMin) ("Target " <> show socMin <> "%")
    (True , _   , _   , _    ) -> Decision (soc < socMax) ("Target " <> show socMax <> "%")
  where forced = case (relayForced relay, respectManual) of
          (_,    Just False) -> False
          (True, _         ) -> True
          _                  -> False
