{-# LANGUAGE TupleSections #-}
module Common.DbHelpers ( singleQuery
                        , withTimeout
                        ) where

import Database.PostgreSQL.Simple
import Control.Exception (Exception, try, throw)
import Common.Timer

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

-- |This wrapper runs given fold until timeout and collects the
-- results. This lets fold to run until timer stops. The consumer is
-- never stopped and the test takes place between consumer
-- invocations. A tuple is returned which has timeout boolean in fst
-- and result on snd. The only oddity is that return type must be an
-- instance of an exception!
withTimeout :: Exception a => Timer -> ((a -> b -> IO a) -> IO a) -> (a -> b -> IO a) -> IO (Bool, a)
withTimeout timer action consumer = catchTimeout $ action $ throwWhenTimeout timer consumer

-- |Wrapper which throws the state out of fold if timeout has occured
throwWhenTimeout :: (Exception c) => Timer -> (a -> b -> IO c) -> a -> b -> IO c
throwWhenTimeout timer act oldState row = do
  newState <- act oldState row
  running <- isTimerRunningIO timer
  if running
    then pure newState
    else throw newState
