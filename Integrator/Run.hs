{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Integrator.Run where

import Control.Exception (Exception)
import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int64)
import Text.Read (readMaybe)
import Control.Monad (void)
import Control.Monad.Extra (whileM)
import Data.Foldable (for_)

import Integrator.Config
import Common.DbHelpers
import Common.ConfigHelpers (readConfigFromArg, whenJust_)
import Common.Timer

data State = State { epoch      :: Scientific
                   , cumulative :: Scientific
                   } deriving (Show, Read)

data Stats = Stats { added   :: !Integer
                   , skipped :: !Integer
                   , timepos :: !Scientific
                   } deriving (Show)

-- Fold state is the state carried over fold, containing both the
-- state stored to the database and user friendly info.
data FoldState = FoldState
  { foldState :: State
  , foldStats :: Stats
  } deriving (Show)

instance Exception FoldState

-- |Load state from db or generate an initial one
stateInit :: Task -> Connection -> IO State
stateInit Task{..} conn = do
  stateIn <- singleQuery conn "execute state_get(?)" [name]
  case stateIn of
    Nothing -> do
      -- Building initial state
      initialIn <- singleQuery conn initial ()
      case initialIn of
        Nothing -> fail "No data"
        Just (epoch, cumulative) -> pure State{..}
    Just (Only a) -> maybe stateFail pure (readMaybe a)
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Task -> Connection -> State -> IO ()
storeState Task{..} conn st = void $ execute conn "EXECUTE state_set(?,?)" (name, show st)

-- |Integrate unprocessed data from database and folding it
integrate :: Task -> Connection -> IO Bool -> State -> IO (Bool, FoldState)
integrate task@Task{..} conn checker st = withTimeout checker folder consumer
  where
    initial = FoldState st (Stats 0 0 0)
    folder = fold conn select [epoch st] initial
    consumer = integrator task conn

-- |Folding function which handles single input row.
integrator
  :: Task
  -> Connection
  -> FoldState
  -> (Scientific, Scientific)
  -> IO FoldState
integrator Task{..} conn (FoldState (State oldTime oldSum) (Stats{..})) (newTime, height) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ FoldState (State newTime newSum) (Stats added (skipped + 1) newTime)
    else do execute conn insert (newTime, newRounded)
            pure $ FoldState (State newTime newSum) (Stats (added + 1) skipped newTime)
  where delta = newTime - oldTime
        area = height * delta
        newSum = oldSum + area
        newRounded = (round newSum) :: Int64

runIntegrator :: Config -> Connection -> IO ()
runIntegrator conf@Config{..} conn = do
  -- Run preparatory SQL
  whenJust_ before $ execute_ conn
  -- Run individual tasks
  for_ tasks $ \task -> do
    -- Run each task in parts if it takes too long otherwise
    whileM $ withTransaction conn $ do
      checkStop <- case txInterval of
        Nothing -> pure $ pure False -- No timeout
        Just t  -> newTarget t >>= pure . isTargetReached
      -- A tranaction reads state and stores it back if successful
      -- (timeout is caught and not considered a failure)
      state <- stateInit task conn
      (hasTimeout, FoldState{..}) <- integrate task conn checkStop state
      storeState task conn foldState
      -- Report to user
      let msg = if hasTimeout then "Processing task " else "Finished task "
          Stats{..} = foldStats
      lastTime <- sqlConvertTimestamp "YYYY-MM-DD HH24:MI" conn timepos
      putStrLn $ msg <> show (name task) <> " up to " <> lastTime <>
        ". Added: " <> show added <> " / " <> show (added + skipped)
      -- Do until fully completes
      pure hasTimeout
  -- Run "after" tasks from config
  whenJust_ after $ execute_ conn
