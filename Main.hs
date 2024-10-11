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
import Sql.Run (prepareSqlRun)

import Control.Applicative

data Config = Config
  { connString  :: ByteString    -- ^PostgreSQL connection string
  , repeatEvery :: Maybe String  -- ^Interval between runs (in PostgreSQL interval format)
  , run         :: [Task]        -- ^Tasks to run sequentially
  } deriving (Generic, Show)

data Task = Task
  { taskType    :: TaskType     -- ^Task to run
  , taskConf    :: FilePath     -- ^Path to its configuration file
  } deriving (Generic, Show)

data TaskType
  = TaskChargeDecision
  | TaskBinner
  | TaskIntegrator
  | TaskSql
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

instance FromJSON Task where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 4}

instance FromJSON TaskType where
  parseJSON = genericParseJSON opts{constructorTagModifier = fieldMangler 4}

type RunTask = Connection -> IO ()

data Tagged a = Tagged
  { task    :: a
  , tag     :: String
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
  putStrLn "# Initializing tasks"
  tasks <- for run $ \Task{..} -> do
    -- Relative path for config files
    let f = takeDirectory mainConfPath </> taskConf
    task <- case taskType of
      TaskChargeDecision -> Y.decodeFileThrow f >>= prepareChargeDecision
      TaskBinner         -> runBinner <$> Y.decodeFileThrow f
      TaskIntegrator     -> runIntegrator <$> Y.decodeFileThrow f
      TaskSql            -> prepareSqlRun f
    -- Embed name for debugging
    let tag = show taskType <> " (" <> taskConf <> ")"
    pure Tagged{..}
  case mbInterval of
    -- Oneshot
    Nothing -> perform conn tasks
    -- Recurring
    Just interval -> do
      initialTarget <- newTarget interval
      flip loopM initialTarget $ \target -> do
        putStrLn "# Starting periodic run"
        perform conn tasks
        putStrLn "# Finished periodic run"
        -- Wait until target is reached and retarget
        waitForTarget target
        pure $ Left $ pushTarget interval target

-- |This is a separate function to show the type more cleanly for
-- humans like you. It takes a list of actions, all needing a database
-- connection.
perform :: Traversable t => Connection -> t (Tagged RunTask) -> IO ()
perform conn tasks = for_ tasks $ \Tagged{..} -> do
  putStrLn $ "## " <> tag
  task conn
  execute_ conn "DEALLOCATE ALL;"
