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
import Binner.Run (runBinner)
import ChargeDecision.Run (prepareChargeDecision)
import Integrator.Run (runIntegrator)

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

type RunTask = Connection -> IO ()

data Tagged a = Tagged
  { item :: a
  , tag  :: String
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
    task <- case taskType of
      TaskChargeDecision -> prepareTask f prepareChargeDecision
      TaskBinner         -> prepareTask f $ pure . runBinner
      TaskIntegrator     -> prepareTask f $ pure . runIntegrator
    -- Embed name for debugging
    pure $ Tagged task $ show taskType <> " (" <> taskConf <> ")"
  perform conn tasks

-- |Parses configuration file for a task and returns a Task.
prepareTask :: (FromJSON a) => FilePath -> (a -> IO RunTask) -> IO RunTask
prepareTask path preparer = do
  conf <- Y.decodeFileThrow path
  preparer conf

-- |This is a separate function to show the type more cleanly for
-- humans like you. It takes a list of actions, all needing a database
-- connection.
perform :: Traversable t => Connection -> t (Tagged RunTask) -> IO ()
perform conn tasks = for_ tasks $ \Tagged{..} -> do
  putStrLn $ "-- " <> tag <> " --"
  item conn
  execute_ conn "DEALLOCATE ALL;"
