{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Integrator.Types ( Config(..)
                        , Task(..)
                        , State(..)
                        , Stats(..)
                        , FoldState(..)
                        , Integration(..)
                        ) where

import Control.Exception (Exception)
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

data State = State { epoch      :: Scientific
                   , cumulative :: Scientific
                   } deriving (Show, Read)

data Stats = Stats { added   :: !Integer
                   , skipped :: !Integer
                   } deriving (Show)

-- Fold state is the state carried over fold, containing both the
-- state stored to the database and user friendly info.
data FoldState = FoldState
  { foldState :: State
  , foldStats :: Stats
  } deriving (Show)

instance Exception FoldState

data Integration = Integration
  { v      :: Integer       -- ^Integrated value
  , dt     :: Scientific    -- ^Delta time in seconds
  , parent :: Maybe Integer -- ^Source row
  } deriving (Generic, Show)

instance ToJSON Integration where
    toEncoding = genericToEncoding defaultOptions
      { fieldLabelModifier = fieldMangler 0
      , omitNothingFields = True
      }
