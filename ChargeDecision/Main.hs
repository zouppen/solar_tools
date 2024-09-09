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

data Config = Config { connString      :: ByteString
                     , sql             :: Query
                     , fullChargeAfter :: String
                     , relayUrl        :: String
                     , debug           :: Maybe Bool
                     } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

data State = State { charging         :: Bool
                   , fullChargeNeeded :: Bool
                   , soc              :: Scientific
                   , profile          :: String
                   , socMin           :: Scientific
                   , socMax           :: Scientific
                   , allowFullCharge  :: Bool
                   , control          :: Maybe Bool -- ^Used in debug print only
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
  dbg $ printBL $ "State: " <> encode state{control = Just shouldCharge}
  out <- writeRelay shouldCharge
  dbg $ printBL $ "Result: " <> out

collectState :: Connection -> RelayReader -> Config -> IO State
collectState conn readRelay Config{..} = do
  -- Read relay information
  (relayInfo, charging) <- readRelay
  -- Insert relayInfo in a separate transaction to make sure data
  -- collection even when control fails.
  withTransaction conn $ execute conn "EXECUTE info(?)" [relayInfo]
  -- In the second transaction, collect charger and profile data
  withTransaction conn $ do
    Just [fullChargeNeeded] <- singleQuery conn "EXECUTE full_charge_needed(?)" [fullChargeAfter]
    Just [soc] <- singleQuery conn "EXECUTE soc" ()
    Just (profile, socMin, socMax, allowFullCharge) <- singleQuery conn "EXECUTE profile" ()
    pure State{control=Nothing,..}

decide :: Config -> State -> Bool
decide Config{..} State{..} = case (charging, fullChargeNeeded, allowFullCharge) of
  (True , True, True) -> soc < 100
  (False, _   , _   ) -> soc < socMin
  (True , _   , _   ) -> soc < socMax
