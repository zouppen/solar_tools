{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Integrator.Config ( Config(..)
                         , Task(..)
                         , readConfig
                         ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.Scientific (Scientific)
import qualified Data.Yaml as Y
import GHC.Generics

import Common.ConfigHelpers

data Config = Config { connString :: ByteString
                     , before     :: Maybe Query
                     , after      :: Maybe Query
                     , txInterval :: Maybe Scientific -- ^Run all tasks in a single transaction if Nothing. Otherwise, commit in a safe point every this seconds
                     , tasks      :: [Task]
                     } deriving (Generic, Show)

data Task = Task { name       :: ByteString
                 , initial    :: Query
                 , select     :: Query
                 , insert     :: Query
                 } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

instance FromJSON Task where
  parseJSON = genericParseJSON opts{fieldLabelModifier = fieldMangler 0}

readConfig :: FilePath -> IO Config
readConfig = Y.decodeFileThrow
