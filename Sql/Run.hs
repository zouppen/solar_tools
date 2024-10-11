module Sql.Run where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.String (fromString)
import Database.PostgreSQL.Simple

prepareSqlRun :: FilePath -> IO (Connection -> IO ())
prepareSqlRun file = do
  str <- readFile file
  q <- evaluate $ fromString str
  -- Return action which runs it
  pure $ \conn -> void $ execute_ conn q
