{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}
module Main where

import System.Environment (getArgs)
import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.Foldable (for_)

import Config
import Common.Any
import Common.DbHelpers

data State = State { epoch      :: Scientific
                   , cumulative :: Scientific
                   } deriving (Show, Read)

data Stats = Stats { added   :: !Integer
                   , skipped :: !Integer
                   } deriving (Show)

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
      undefined
    Just (Only a) -> maybe stateFail pure (readMaybe a)
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Task -> Connection -> State -> IO ()
storeState Task{..} conn st = void $ execute conn "EXECUTE state_set(?,?)" (name, show st)

-- |Integrate unprocessed data from database and folding it
integrate :: Task -> Connection -> State -> IO (State, Stats)
integrate task@Task{..} conn st = fold conn select
  [epoch st] (st, Stats 0 0) (integrator task conn)

-- |Folding function which handles single input row.
integrator
  :: Task
  -> Connection
  -> (State, Stats)
  -> (Any, Scientific, Scientific)
  -> IO (State, Stats)
integrator Task{..} conn (State oldTime oldSum, Stats{..}) (id, newTime, height) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ (State newTime newSum, Stats added (skipped + 1))
    else do execute conn insert (id, newRounded)
            pure (State newTime newSum, Stats (added + 1) skipped)
  where delta = newTime - oldTime
        area = height * delta
        newSum = oldSum + area
        newRounded = (round newSum) :: Int64

maybeRun :: Monad m => Maybe t -> (t -> m a) -> m ()
maybeRun Nothing _ = pure ()
maybeRun (Just a) f = f a >> pure ()

main :: IO ()
main = do
  args <- getArgs
  conf@Config{..} <- case args of
    [confPath] -> readConfig confPath
    _ -> fail $ "Give configuration file as the only argument"
  let singleTx' = singleTx /= Just False -- Default True
  conn <- connectPostgreSQL $ connString
  (if singleTx' then withTransaction conn else id) $ do
    maybeRun before $ execute_ conn
    for_ tasks $ \task -> (if not singleTx' then withTransaction conn else id) $ do
      state <- stateInit task conn
      (newState, stats) <- integrate task conn state
      storeState task conn newState
      putStrLn $ "Finished task " ++ show (name task) ++ ". " ++ show stats
    maybeRun after $ execute_ conn
