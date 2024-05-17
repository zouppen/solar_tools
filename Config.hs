{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Config ( Config(..)
              , Task(..)
              , PrepareQuery(..)
              , readConfig
              ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.String (fromString)
import qualified Data.Yaml as Y
import GHC.Generics

data Config = Config { connString :: ByteString
                     , stateGet   :: PrepareQuery
                     , stateSet   :: PrepareQuery
                     , tasks      :: [Task]
                     } deriving (Generic, Show)

data Task = Task { name       :: ByteString
                 , initialGet :: Query
                 , select     :: Query
                 , insert     :: Query
                 } deriving (Generic, Show)

newtype PrepareQuery = PrepareQuery {getPrepare :: String -> Query}

instance FromJSON Query where
  parseJSON a = fromString <$> parseJSON a

instance FromJSON ByteString where
  parseJSON a = fromString <$> parseJSON a

instance FromJSON Config where
  parseJSON = genericParseJSON opts

instance FromJSON Task where
  parseJSON = genericParseJSON opts

instance FromJSON PrepareQuery where
  parseJSON s = PrepareQuery . wrap <$> parseJSON s
    where wrap stmt name = fromString $ "PREPARE " ++ name ++ " AS " ++ stmt

instance Show PrepareQuery where
  show (PrepareQuery a) = show $ a "name"

opts = defaultOptions { rejectUnknownFields = True
                      }

readConfig :: FilePath -> IO Config
readConfig = Y.decodeFileThrow
