module Common.ConfigHelpers where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.String (fromString)
import System.Environment (getArgs)
import Control.Monad (void)

instance FromJSON Query where
  parseJSON a = fromString <$> parseJSON a

instance FromJSON ByteString where
  parseJSON a = fromString <$> parseJSON a

configHelper parser = do
  args <- getArgs
  case args of
    [confPath] -> parser confPath
    _ -> fail $ "Give configuration file as the only argument"

opts = defaultOptions { rejectUnknownFields = True
                      }

-- |Allows stripping first letters from field name and camel-casing the rest
fieldMangler :: Int -> String -> String
fieldMangler n = camelTo2 '_' . drop n

-- |Like whenJust from extra but ignores the result.
whenJust_ :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust_ mg f =  maybe (pure ()) (void.f) mg
