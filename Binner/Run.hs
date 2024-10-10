{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveAnyClass #-}
module Binner.Run where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Extra (whileM)
import Data.Aeson
import Data.Scientific
import Database.PostgreSQL.Simple
import GHC.Generics

import Common.Timer
import Common.Binning
import Common.DbHelpers
import Common.ConfigHelpers

data Config = Config
  { before     :: Maybe Query      -- ^SQL to run before operation
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

data BinResult = BinResult { binned  :: !Int
                           , skipped :: !Int
                           } deriving (Show)

instance Exception BinResult

runBinner :: Config -> Connection -> IO ()
runBinner config@Config{..} conn = do
  let dbg = when (debug == Just True)
  -- Run preparatory SQL
  whenJust_ before $ execute_ conn
  -- Timer which allows us to do it incrementally
  timer <- case txInterval of
      Nothing -> newInfiniteTimer
      Just t  -> newTimer t
  whileM $ withTransaction conn $ do
    atomically $ startTimer timer
    let f = fold_ conn select (BinResult 0 0)
    (timeout, stats) <- withTimeout timer f $ \BinResult{..} InputRow{..} -> do
      case mbPrevTime of
        Nothing -> do
          dbg $ putStrLn $ "Skipping first event, id=" <> show eventId
          pure BinResult{skipped = skipped+1, ..}
        Just prevTime -> do
          execute conn update (eventId, toBin prevTime curTime)
          pure BinResult{binned = binned+1, ..}
    -- Show signs of life
    let extra = if timeout then " Continuing..." else ""
    putStrLn $ show stats <> extra
    -- Keep on running if needed
    pure timeout
