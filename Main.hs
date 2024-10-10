{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
-- |Runs given tools sequentially
module Main where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Traversable (for)
import Data.Foldable (for_)
import qualified Data.Yaml as Y
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_)
import GHC.Generics
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.FilePath (takeDirectory, (</>))

import Common.ConfigHelpers
import Binner.Binner (runBinner)
import ChargeDecision.ChargeDecision (runChargeDecision)
import Integrator.Integrator (runIntegrator)

data Config = Config
  { connString :: ByteString    -- ^PostgreSQL connection string
  , run        :: [Task]        -- ^Tasks to run sequentially
  } deriving (Generic, Show)

data Task = Task
  { taskType :: TaskType        -- ^Task to run
  , taskConf :: FilePath        -- ^Path to its configuration file
  } deriving (Generic, Show)

data TaskType
  = TaskChargeDecision
  | TaskBinner
  | TaskIntegrator
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

instance FromJSON Task where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 4}

instance FromJSON TaskType where
  parseJSON = genericParseJSON opts{constructorTagModifier = fieldMangler 4}

data RunTask = RunTask
  { act  :: Connection -> IO ()
  , name :: String
  }

main = do
  -- Prevent too long output buffers
  hSetBuffering stdout LineBuffering
  -- We need the file name so we use this instead of readConfigFromArg
  mainConfPath <- argParse1 "Give configuration file as the only argument"
  Config{..} <- Y.decodeFileThrow mainConfPath
  -- The DB connection is common to all
  conn <- connectPostgreSQL connString
  tasks <- for run $ \Task{..} -> do
    -- Relative path for config files
    let f = takeDirectory mainConfPath </> taskConf
    protoRunTask <- case taskType of
      TaskChargeDecision -> prepareTask f runChargeDecision
      TaskBinner         -> prepareTask f runBinner
      TaskIntegrator     -> prepareTask f runIntegrator
    -- Embed name for debugging
    pure $ protoRunTask $ show taskType
  perform conn tasks

-- |Parses configuration file for a task and returns a Task.
prepareTask :: (FromJSON a) => FilePath -> (Connection -> a -> IO ()) -> IO (String -> RunTask)
prepareTask path fun = do
  conf <- Y.decodeFileThrow path
  pure $ RunTask $ flip fun conf

-- |This is a separate function to show the type more cleanly for
-- humans like you. It takes a list of actions, all needing a database
-- connection.
perform :: Traversable t => Connection -> t RunTask -> IO ()
perform conn tasks = for_ tasks $ \RunTask{..} -> do
  putStrLn $ "-- " <> name <> " --"
  act conn
  execute_ conn "DEALLOCATE ALL;"
