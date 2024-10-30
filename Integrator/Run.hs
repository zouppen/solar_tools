{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Integrator.Run where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Newtypes (Aeson(..))
import Data.Scientific (Scientific)
import Text.Read (readMaybe)
import Control.Monad (void)
import Control.Monad.Extra (whileM)
import Data.Foldable (for_)

import Integrator.Types
import Common.DbHelpers
import Common.ConfigHelpers (whenJust_)
import Common.Timer

-- |Load state from db or generate an initial one
stateInit :: Task -> Connection -> IO State
stateInit Task{..} conn = do
  stateIn <- query conn "execute state_get(?)" [name]
  case stateIn of
    []    -> pure State{ epoch = 0 -- i.e. 1970-01-01
                       , cumulative = Nothing
                       , duplicate = Nothing
                       }
    [[a]] -> maybe stateFail pure (readMaybe a)
    _     -> fail $ "State query for " <> show name <> " returns garbage"
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Task -> Connection -> State -> IO ()
storeState Task{..} conn st = void $ execute conn "EXECUTE state_set(?,?)" (name, show st)

-- |Integrate unprocessed data from database and folding it
integrate :: Task -> Connection -> IO Bool -> State -> IO (Bool, FoldState)
integrate task@Task{..} conn checker st = withTimeout checker folder consumer
  where
    initial = FoldState st mempty
    folder = fold conn select [epoch st] initial
    consumer = integrator task conn

-- |Folding function which handles single input row.
integrator
  :: Task
  -> Connection
  -> FoldState
  -> (Maybe Integer, Scientific, Scientific)
  -> IO FoldState
integrator Task{..} conn (FoldState (State oldTime oldSum oldDup) oldStats) (parent, newTime, height) = do
  -- Inserting data. Do not insert if it didn't increment
  (newDup, stat) <- case (isFresh, oldDup) of
    (True, _) -> do
      -- Insert the value and do not mark as duplicate
      void $ execute conn insert (newTime, Aeson Integration{..})
      pure (Nothing, mempty{added = 1})
    (_, Nothing) -> do
      -- Inserting the duplicate and mark it so
      i <- query conn insert (newTime, Aeson Integration{..}) >>= fetchId
      pure (Just i, mempty{addedDup = 1})
    (_, Just i) -> do
      -- We know a dup already, so updating the end time
      void $ execute conn update (i, newTime)
      pure (Just i, mempty{skipped = 1})
  -- And crafting the return value
  pure FoldState{ foldState = State newTime (Just newSum) newDup -- Unrounded
                , foldStats = oldStats <> stat
                }
  where dt = newTime - oldTime
        area = height * dt
        v = round newSum -- For database
        newSum = case oldSum of
          Nothing  -> 0 -- Start integration from 0
          Just old -> old + area
        isFresh = case oldSum of
          Nothing  -> True -- Initial value is always fresh
          Just old -> round old /= v
        fetchId [[a]] = pure a
        fetchId _     = fail "Insert statement doesn't return id as expected"

runIntegrator :: Config -> Connection -> IO ()
runIntegrator Config{..} conn = do
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
      lastTime <- sqlConvertTimestamp "YYYY-MM-DD HH24:MI" conn (epoch foldState)
      putStrLn $ msg <> show (name task) <> " up to " <> lastTime <>
        ". Added: " <> show added <> " / " <> show (added + skipped + addedDup) <> "(" <> show addedDup <> " duplicates)"
      -- Do until fully completes
      pure hasTimeout
  -- Run "after" tasks from config
  whenJust_ after $ execute_ conn
