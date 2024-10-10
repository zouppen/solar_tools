{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module ChargeDecision.Run where

import Control.Monad (when, unless)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Scientific
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Newtypes (Aeson(..))
import GHC.Generics
import System.Exit

import Common.DbHelpers
import Common.ConfigHelpers
import Common.Relay
import Common.Shelly

data Config = Config
  { sql             :: ConfigSql  -- ^SQL to run
  , fullChargeAfter :: String     -- ^How often battery should reach 100%
  , respectManual   :: Maybe Bool -- ^Keep charger on if manually started (default: on)
  , relayUrl        :: String     -- ^Shelly relay URL
  , debug           :: Maybe Bool -- ^Do debug printing (default: off)
  } deriving (Generic, Show)

data ConfigSql = ConfigSql
  { sqlPrepare      :: Query
  , sqlDecision     :: Maybe Query
  , sqlAlert        :: Maybe Query
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

instance FromJSON ConfigSql where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 3}

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
    toEncoding = genericToEncoding defaultOptions{fieldLabelModifier = fieldMangler 0}

data Decision = Decision
  { decision    :: Bool
  , target      :: Maybe Scientific
  , explanation :: String
  } deriving (Generic, Show)

instance ToJSON Decision where
    toEncoding = genericToEncoding defaultOptions

prepareChargeDecision :: Config -> IO (Connection -> IO ())
prepareChargeDecision conf@Config{..} = do
  -- Prepare relay control
  relay <- initShelly relayUrl
  -- Prepopulate relay part
  pure $ runChargeDecision relay conf

runChargeDecision :: Relay -> Config -> Connection -> IO ()
runChargeDecision Relay{..} config@Config{..} conn = do
  let dbg = when (debug == Just True)
  -- Run preparatory SQL
  execute_ conn $ sqlPrepare sql
  -- Get current state of things
  state@State{..} <- collectState conn readRelay config
  let dec@Decision{..} = decide config state
  dbg $ BL.putStrLn $ "State: " <> encode state
  let (doControl, alert, why) = case (relayState relay, decision, relayMode relay) of
        (_, _, RelayBooted)  -> (True,  True,  "Power resumed")
        (_, _, RelayTimeout) -> (True,  True,  "Relay timeout detected")
        (True, True, _)      -> (False, False, "Already on")
        (False, False, _)    -> (True,  False, "Heartbeat")
        _                    -> (True,  False, "State change")
  dbg $ putStrLn $
    "Decision: " <> show decision <>
    "\nExplanation: " <> explanation <>
    "\nAction: " <> why
  -- Store decision
  case (sqlAlert sql, alert) of
    (Just q, True) -> withTransaction conn $ execute conn q [why]
    _ -> pure 0
  case sqlDecision sql of
    Just q -> withTransaction conn $ execute conn q [Aeson dec]
    Nothing -> pure 0
  when doControl $ do
    out <- writeRelay decision
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
    (True , _   , _   , True ) -> Decision True Nothing "Manual mode"
    (True , True, True, _    ) -> Decision (soc < 100) (Just 100) "Full charge"
    (False, _   , _   , _    ) -> Decision (soc < socMin) (Just socMin) "Low mark"
    (True , _   , _   , _    ) -> Decision (soc < socMax) (Just socMax) "High mark"
  where forced = case (relayMode relay, respectManual) of
          (_          , Just False) -> False
          (RelayManual, _         ) -> True
          _                         -> False
