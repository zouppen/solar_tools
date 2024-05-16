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

data State = State { epoch      :: Scientific
                   , cumulative :: Scientific
                   } deriving (Show, Read)

data Stats = Stats { added   :: !Integer
                   , skipped :: !Integer
                   } deriving (Show)


-- |Helper to handle getting initial values, containing only one single answer row
singleQuery :: FromRow a => Connection -> IO b -> (a -> IO b) -> Query -> IO b
singleQuery conn whenNone whenOne q = do
  ans <- query_ conn q
  case ans of
    []  -> whenNone
    [a] -> whenOne a
    _   -> fail $ "More than one answer to this query: " ++ show q

-- |Load state from db or generate an initial one
stateInit :: Task -> Connection -> IO State
stateInit Task{..} conn = do
  let none = do
        let none = fail "No data in 'aurinko' table"
            one [e] = pure $ State e 0
          in singleQuery conn none one initialGet
      one [a] = maybe stateFail pure (readMaybe a)
    in singleQuery conn none one stateGet
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Task -> Connection -> State -> IO ()
storeState Task{..} conn st = void $ execute conn stateSet [show st]

-- |Integrate unprocessed data from database and folding it
integrate :: Task -> Connection -> State -> IO (State, Stats)
integrate task@Task{..} conn st = fold conn select
  [epoch st] (st, Stats 0 0) (integrator task conn)

-- |Folding function which handles single input row.
integrator
  :: Task
  -> Connection
  -> (State, Stats)
  -> (Int32, Scientific, Scientific)
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

main :: IO ()
main = do
  args <- getArgs
  Config{..} <- case args of
    [confPath] -> readConfig confPath
    _ -> fail $ "Give configuration file as the only argument"
  conn <- connectPostgreSQL $ connString
  withTransaction conn $ for_ tasks $ \task -> do
    state <- stateInit task conn
    (newState, stats) <- integrate task conn state
    storeState task conn newState
    let str = maybe "Finished task. " (\s -> "Finished " ++ s ++ ". ") (name task)
    putStrLn $ str ++ show stats
