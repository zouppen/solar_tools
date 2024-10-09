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

-- |Uses the config file supplied on command line and run the parser
configHelper :: (FilePath -> IO a) -> IO a
configHelper parser = getConfigFile >>= parser

-- |Gets the config file location from command line.
getConfigFile :: IO FilePath
getConfigFile = do
  args <- getArgs
  case args of
    [confPath] -> pure confPath
    _ -> fail $ "Give configuration file as the only argument"

opts = defaultOptions { rejectUnknownFields = True
                      }

-- |Allows stripping first letters from field name and camel-casing the rest
fieldMangler :: Int -> String -> String
fieldMangler n = camelTo2 '_' . drop n

-- |Like whenJust from extra but ignores the result.
whenJust_ :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust_ mg f =  maybe (pure ()) (void.f) mg
