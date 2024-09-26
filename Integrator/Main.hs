{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Main where

import Control.Exception (Exception, throw, try)
import Database.PostgreSQL.Simple
import Data.Function (fix)
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)
import Control.Monad (void, when)
import Data.Foldable (for_)

import Integrator.Config
import Common.Any
import Common.DbHelpers
import Common.ConfigHelpers (configHelper)
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
integrate :: Task -> Connection -> Timer -> State -> IO FoldState
integrate task@Task{..} conn timer st = fold conn select
  [epoch st] (FoldState st (Stats 0 0)) (timeoutWrapper timer (integrator task conn))

-- |Wrapper which throws the state out of fold if timeout has occured
timeoutWrapper :: (Exception c) => Timer -> (a -> b -> IO c) -> a -> b -> IO c
timeoutWrapper timer act oldState row = do
  newState <- act oldState row
  running <- isTimerRunningIO timer
  if running
    then pure newState
    else throw newState

-- |Folding function which handles single input row.
integrator
  :: Task
  -> Connection
  -> FoldState
  -> (Any, Scientific, Scientific)
  -> IO FoldState
integrator Task{..} conn (FoldState (State oldTime oldSum) (Stats{..})) (id, newTime, height) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ FoldState (State newTime newSum) (Stats added (skipped + 1))
    else do execute conn insert (id, newRounded)
            pure $ FoldState (State newTime newSum) (Stats (added + 1) skipped)
  where delta = newTime - oldTime
        area = height * delta
        newSum = oldSum + area
        newRounded = (round newSum) :: Int64

maybeRun :: Monad m => Maybe t -> (t -> m a) -> m ()
maybeRun Nothing _ = pure ()
maybeRun (Just a) f = f a >> pure ()

-- |This weird wrapper takes an action and catches only exceptions of
-- same type than return value. This is a corner case of the timeout
-- where the return type and exception are carrying the same
-- payload. The fact if exception has catched is stored to the fst of
-- the tuple.
catchTimeout :: Exception a => IO a -> IO (Bool, a)
catchTimeout act = either (True,) (False,) <$> try act

main :: IO ()
main = do
  conf@Config{..} <- configHelper readConfig
  conn <- connectPostgreSQL $ connString
  -- Start transaction and run preparing statements from config
  maybeRun before $ execute_ conn
  -- Run individual tasks
  for_ tasks $ \task -> do
    -- Initialize timer based on config
    timer <- case txInterval of
      Nothing      -> newInfiniteTimer
      Just timeout -> newTimer timeout
    fix $ \loop -> do
      atomically $ startTimer timer
      -- A tranaction reads state and stores it back if successful
      -- (timeout is caught and not considered a failure)
      (hasTimeout, foldStats) <- withTransaction conn $ do
        state <- stateInit task conn
        (hasTimeout, FoldState{..}) <- catchTimeout $ integrate task conn timer state
        storeState task conn foldState
        pure (hasTimeout, foldStats)
      -- Report to user
      let msg = if hasTimeout then "Processing task " else "Finished task "
      putStrLn $ msg ++ show (name task) ++ ". " ++ show foldStats
      -- Do until fully completes
      when hasTimeout loop
  -- Run "after" tasks from config and commit everything
  maybeRun after $ execute_ conn
