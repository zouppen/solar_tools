{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Main where

import Database.PostgreSQL.Simple
import Data.ByteString (ByteString)
import Data.Scientific
import Data.Aeson
import GHC.Generics
import Network.Curl.Aeson
import qualified Data.Yaml as Y

import Common.DbHelpers
import Common.ConfigHelpers

data Config = Config { connString      :: ByteString
                     , socMin          :: Scientific
                     , socMax          :: Scientific
                     , fullChargeAfter :: String
                     , relayUrl        :: String
                     } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

data State = State { charging         :: Bool
                   , fullChargeNeeded :: Bool
                   , soc              :: Scientific
                   } deriving (Show, Eq)

main :: IO ()
main = do
  config@Config{..} <- configHelper Y.decodeFileThrow
  state@State{..} <- dbRead config
  let shouldCharge = decide config state
  if charging /= shouldCharge
    then do putStrLn $ "State change to " <> show shouldCharge
            control config shouldCharge >>= print
    else pure ()

control :: Config -> Bool -> IO Value
control Config{..} x = curlAesonCustom mempty "POST" (relayUrl <> "/rpc/Switch.Set") payload
  where payload = Just $ object ["id" .= (0::Int), "on" .= x]

dbRead :: Config -> IO State
dbRead Config{..} = do
  conn <- connectPostgreSQL connString
  withTransaction conn $ do
    Just (Only charging) <- singleQuery conn "select (value->'output')::boolean from event where name='charger' order by ts desc limit 1" ()
    Just (Only fullChargeNeeded) <- singleQuery conn "select (interval ? < current_timestamp-ts) from event where name='victron' and (value->>'name')='Akkumittari' and (value->'payload'->>'soc')::numeric=100 order by ts desc limit 1" [fullChargeAfter]
    Just (Only soc) <- singleQuery conn "select floor((value->'payload'->'soc')::numeric) from event where name='victron' and (value->>'name')='Akkumittari' order by ts desc limit 1" ()
    pure State{..}

decide :: Config -> State -> Bool
decide Config{..} State{..} = case (charging, fullChargeNeeded, soc) of
  (False, _, soc)    -> soc < socMin
  (True, False, soc) -> soc < socMax
  (True, True, soc)  -> soc < 100
