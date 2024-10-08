{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Extra (whileM)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Scientific
import qualified Data.Yaml as Y
import Database.PostgreSQL.Simple
import GHC.Generics

import Common.Timer
import Common.Binning
import Common.DbHelpers
import Common.ConfigHelpers

data Config = Config
  { connString :: ByteString       -- ^PostgreSQL connection string
  , before     :: Maybe Query      -- ^SQL to run before operation
  , select     :: Query            -- ^SQL which returns: id, prev, current
  , update     :: Query            -- ^SQL which takes: id, bin
  , debug      :: Maybe Bool       -- ^Do debug printing (default: off)
  , txInterval :: Maybe Scientific -- ^Commit in a safe point every
                                   -- this seconds. (default: commit
                                   -- in the end)
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

data InputRow = InputRow
  { eventId    :: Integer
  , mbPrevTime :: Maybe Scientific
  , curTime    :: Scientific
  } deriving (Generic, FromRow, Show)

newtype BinResult = BinResult { binned :: Int } deriving (Show)

instance Exception BinResult

main :: IO ()
main = do
  config@Config{..} <- configHelper Y.decodeFileThrow
  let dbg = when (debug == Just True)
  -- Connect to database and run preparatory SQL
  conn <- connectPostgreSQL connString
  whenJust_ before $ execute_ conn
  -- Timer which allows us to do it incrementally
  timer <- case txInterval of
      Nothing -> newInfiniteTimer
      Just t  -> newTimer t
  whileM $ withTransaction conn $ do
    atomically $ startTimer timer
    let f = fold_ conn select (BinResult 0)
    (timeout, BinResult{..}) <- withTimeout timer f $ \BinResult{..} InputRow{..} -> do
      case mbPrevTime of
        Nothing -> dbg $ putStrLn $ "Skipping first event, id=" <> show eventId
        Just prevTime -> do
          void $ execute conn update (eventId, toBin prevTime curTime)
      pure $ BinResult $ binned `seq` binned + 1
    -- Show signs of life
    let msg = if timeout then " Continuing..." else ""
    putStrLn $ "Processed " <> show binned <> " events." <> msg
    -- Keep on running if needed
    pure timeout
