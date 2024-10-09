{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Integrator.Config ( Config(..)
                         , Task(..)
                         ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.Scientific (Scientific)
import GHC.Generics

import Common.ConfigHelpers

data Config = Config { before     :: Maybe Query
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
