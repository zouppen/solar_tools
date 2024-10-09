{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Integrator.Integrator where

import Control.Exception (Exception, throw, try)
import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
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
integrate :: Task -> Connection -> Timer -> State -> IO (Bool, FoldState)
integrate task@Task{..} conn timer st = withTimeout timer (fold conn select
  [epoch st] (FoldState st (Stats 0 0))) (integrator task conn)

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
    then pure $ FoldState (State newTime newSum) (Stats added (skipped + 1))
    else do execute conn insert (newTime, newRounded)
            pure $ FoldState (State newTime newSum) (Stats (added + 1) skipped)
  where delta = newTime - oldTime
        area = height * delta
        newSum = oldSum + area
        newRounded = (round newSum) :: Int64

runIntegrator sharedDb conf@Config{..} = do
  conn <- connectSharedDb sharedDb connString
  -- Start transaction and run preparing statements from config
  whenJust_ before $ execute_ conn
  -- Run individual tasks
  for_ tasks $ \task -> do
    -- Initialize timer based on config
    timer <- case txInterval of
      Nothing      -> newInfiniteTimer
      Just timeout -> newTimer timeout
    whileM $ withTransaction conn $ do
      atomically $ startTimer timer
      -- A tranaction reads state and stores it back if successful
      -- (timeout is caught and not considered a failure)
      state <- stateInit task conn
      (hasTimeout, FoldState{..}) <- integrate task conn timer state
      storeState task conn foldState
      -- Report to user
      let msg = if hasTimeout then "Processing task " else "Finished task "
      putStrLn $ msg ++ show (name task) ++ ". " ++ show foldStats
      -- Do until fully completes
      pure hasTimeout
  -- Run "after" tasks from config and commit everything
  whenJust_ after $ execute_ conn
