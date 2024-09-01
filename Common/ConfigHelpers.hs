module Common.ConfigHelpers where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.String (fromString)

instance FromJSON Query where
  parseJSON a = fromString <$> parseJSON a

instance FromJSON ByteString where
  parseJSON a = fromString <$> parseJSON a

opts = defaultOptions { rejectUnknownFields = True
                      }
