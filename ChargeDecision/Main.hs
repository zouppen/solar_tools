{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Main where

import Control.Monad (when)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Scientific
import qualified Data.Yaml as Y
import Database.PostgreSQL.Simple
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
  let shouldCharge = decide config state
  dbg $ printBL $ "State: " <> encode state
  case (shouldCharge, relayState relay) of
    (Nothing, _)      -> dbg $ printBL "Manual mode on"
    (Just True, True) -> dbg $ printBL "Already on"
    (Just a, _) -> do
      dbg $ putStrLn $ "Control to " <> show a
      out <- writeRelay a
      dbg $ printBL $ "Result: " <> out

-- |Connect to database and relay and collect current state
collectState :: Connection -> RelayReader -> Config -> IO State
collectState conn readRelay Config{..} = do
  -- Read relay information
  relay <- readRelay
  -- Insert relayInfo in a separate transaction to make sure data
  -- collection even when control fails.
  withTransaction conn $ execute conn "EXECUTE info(?)" [raw relay]
  -- In the second transaction, collect charger and profile data
  withTransaction conn $ do
    Just [fullChargeNeeded] <- singleQuery conn "EXECUTE full_charge_needed(?)" [fullChargeAfter]
    Just [soc] <- singleQuery conn "EXECUTE soc" ()
    Just (profile, socMin, socMax, allowFullCharge) <- singleQuery conn "EXECUTE profile" ()
    pure State{..}

-- |Do control deceision based on configuration and current state.
decide :: Config -> State -> Maybe Bool
decide Config{..} State{..} =
  case (relayState relay, fullChargeNeeded, allowFullCharge, forced) of
    (True, _    , _   , True ) -> Nothing -- Never override manual start
    (True , True, True, _    ) -> Just $ soc < 100
    (False, _   , _   , _    ) -> Just $ soc < socMin
    (True , _   , _   , _    ) -> Just $ soc < socMax
  where forced = case (relayForced relay, respectManual) of
          (_,         Just False) -> False
          (Just True, _         ) -> True
          _                       -> False
