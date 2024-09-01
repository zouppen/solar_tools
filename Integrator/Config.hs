{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Integrator.Config ( Config(..)
                         , Task(..)
                         , readConfig
                         ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import qualified Data.Yaml as Y
import GHC.Generics

import Common.ConfigHelpers

data Config = Config { connString :: ByteString
                     , before     :: Maybe Query
                     , after      :: Maybe Query
                     , singleTx   :: Maybe Bool  -- ^Run all tasks in a single transaction
                     , tasks      :: [Task]
                     } deriving (Generic, Show)

data Task = Task { name       :: ByteString
                 , initial    :: Query
                 , select     :: Query
                 , insert     :: Query
                 } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts

instance FromJSON Task where
  parseJSON = genericParseJSON opts

readConfig :: FilePath -> IO Config
readConfig = Y.decodeFileThrow
