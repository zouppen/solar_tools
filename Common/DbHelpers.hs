{-# LANGUAGE TupleSections #-}
module Common.DbHelpers where

import Database.PostgreSQL.Simple
import Control.Exception (Exception, try)

-- |Helper to handle getting initial values, containing only one single answer row
singleQuery :: (ToRow r, FromRow a) => Connection -> Query -> r -> IO (Maybe a)
singleQuery conn q r = do
  ans <- query conn q r
  case ans of
    []  -> pure Nothing
    [a] -> pure $ Just a
    _   -> fail $ "More than one result to this query: " ++ show q

-- |This weird wrapper takes an action and catches only exceptions of
-- same type than return value. This is a corner case of the timeout
-- where the return type and exception are carrying the same
-- payload. The fact if exception has catched is stored to the fst of
-- the tuple.
catchTimeout :: Exception a => IO a -> IO (Bool, a)
catchTimeout act = either (True,) (False,) <$> try act
