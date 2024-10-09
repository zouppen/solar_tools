module Common.ConfigHelpers where

import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Query)
import Data.String (fromString)
import qualified Data.Yaml as Y
import System.Environment (getArgs)
import Control.Monad (void)

instance FromJSON Query where
  parseJSON a = fromString <$> parseJSON a

instance FromJSON ByteString where
  parseJSON a = fromString <$> parseJSON a

-- |Uses the config file supplied on command line and runs the parser.
readConfigFromArg :: FromJSON a => IO a
readConfigFromArg = argParse1 errMsg >>= Y.decodeFileThrow
  where errMsg = "Give configuration file as the only argument"

-- |Naïve way to get one argument from the command line
argParse1 :: String -> IO String
argParse1 errMsg = do
  args <- getArgs
  case args of
    [a] -> pure a
    _ -> fail errMsg

opts = defaultOptions { rejectUnknownFields = True
                      }

-- |Allows stripping first letters from field name and camel-casing the rest
fieldMangler :: Int -> String -> String
fieldMangler n = camelTo2 '_' . drop n

-- |Like whenJust from extra but ignores the result.
whenJust_ :: Applicative f => Maybe a -> (a -> f b) -> f ()
whenJust_ mg f =  maybe (pure ()) (void.f) mg
