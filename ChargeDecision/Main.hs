{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Main where

import Control.Exception (SomeException, catch)
import Database.PostgreSQL.Simple
import Data.ByteString (ByteString)
import Data.Scientific
import Data.Aeson
import GHC.Generics
import Network.Curl.Aeson
import qualified Data.Yaml as Y
import System.Exit

import Common.DbHelpers
import Common.ConfigHelpers

data Config = Config { connString      :: ByteString
                     , sql             :: Query
                     , fullChargeAfter :: String
                     , relayUrl        :: String
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
                   } deriving (Show, Eq)

main :: IO ()
main = do
  config@Config{..} <- configHelper Y.decodeFileThrow
  let
    action = do
      state@State{..} <- dbRead config
      let shouldCharge = decide config state
      if charging /= shouldCharge
        then do putStr $
                  "State: " <> show state <> "\n" <>
                  "Controlling to: " <> show shouldCharge <> "\nResult: "
                control config shouldCharge >>= print
        else pure ()
    failure e = do
      putStrLn "Due to an error, forcing charger on unconditionally. Error was:"
      print (e :: SomeException)
      control config True >>= print
      exitFailure
    in catch action failure

control :: Config -> Bool -> IO Value
control Config{..} x = curlAesonCustom mempty "POST" (relayUrl <> "/rpc/Switch.Set") payload
  where payload = Just $ object ["id" .= (0::Int), "on" .= x]

dbRead :: Config -> IO State
dbRead Config{..} = do
  conn <- connectPostgreSQL connString
  withTransaction conn $ do
    execute_ conn sql
    Just [charging] <- singleQuery conn "EXECUTE charging" ()
    Just [fullChargeNeeded] <- singleQuery conn "EXECUTE full_charge_needed(?)" [fullChargeAfter]
    Just [soc] <- singleQuery conn "EXECUTE soc" ()
    Just (profile, socMin, socMax, allowFullCharge) <- singleQuery conn "EXECUTE profile" ()
    pure State{..}

decide :: Config -> State -> Bool
decide Config{..} State{..} = case (charging, fullChargeNeeded, allowFullCharge) of
  (True , True, True) -> soc < 100
  (False, _   , _   ) -> soc < socMin
  (True , _   , _   ) -> soc < socMax
