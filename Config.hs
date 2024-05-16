{-# LANGUAGE OverloadedStrings #-}
module Config ( Config(..)
              , readConfig
              ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.String (fromString)
import qualified Data.Yaml as Y

data Config = Config { connString :: ByteString
                     , stateGet   :: Query
                     , stateSet   :: Query
                     , initialGet :: Query
                     , select     :: Query
                     , insert     :: Query
                     } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    let str k = fromString <$> v .: k
    in Config
       <$> str "connString"
       <*> str "stateGet"
       <*> str "stateSet"
       <*> str "initialGet"
       <*> str "select"
       <*> str "insert"

readConfig :: FilePath -> IO Config
readConfig = Y.decodeFileThrow
