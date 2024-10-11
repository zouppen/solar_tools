{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
-- |Runs given tools sequentially
module Main where

import Control.Monad.Extra (loopM)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Traversable (for)
import Data.Foldable (for_)
import qualified Data.Yaml as Y
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute_)
import GHC.Generics
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.FilePath (takeDirectory, (</>))

import Common.DbHelpers
import Common.ConfigHelpers
import Common.Timer
import Binner.Run (runBinner)
import ChargeDecision.Run (prepareChargeDecision)
import Integrator.Run (runIntegrator)

import Control.Applicative

data Config = Config
  { connString  :: ByteString    -- ^PostgreSQL connection string
  , repeatEvery :: Maybe String  -- ^Interval between runs (in PostgreSQL interval format)
  , run         :: [Task]        -- ^Tasks to run sequentially
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
  -- We use the database to convert timestamps :-D
  mbInterval <- traverse (sqlConvertInterval conn) repeatEvery
  tasks <- for run $ \Task{..} -> do
    -- Relative path for config files
    let f = takeDirectory mainConfPath </> taskConf
    task <- case taskType of
      TaskChargeDecision -> prepareTask f prepareChargeDecision
      TaskBinner         -> prepareTask f $ pure . runBinner
      TaskIntegrator     -> prepareTask f $ pure . runIntegrator
    -- Embed name for debugging
    pure $ Tagged task $ show taskType <> " (" <> taskConf <> ")"
  case mbInterval of
    -- Oneshot
    Nothing -> perform conn tasks
    -- Recurring
    Just interval -> do
      initialTarget <- newTarget interval
      flip loopM initialTarget $ \target -> do
        putStrLn "-- Starting periodic run --"
        perform conn tasks
        -- Wait until target is reached and retarget
        waitForTarget target
        pure $ Left $ pushTarget interval target

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
