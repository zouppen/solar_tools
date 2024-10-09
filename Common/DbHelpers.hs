{-# LANGUAGE TupleSections #-}
module Common.DbHelpers ( SharedConnection
                        , singleQuery
                        , withTimeout
                        , initSharedDb
                        , connectSharedDb
                        ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Control.Exception (Exception, try, throw, onException)
import Common.Timer
import Data.Map.Lazy (Map, (!?), insert)
import Control.Concurrent.STM

newtype SharedConnection = SharedConnection (TMVar (Map ByteString Connection))

initSharedDb :: IO SharedConnection
initSharedDb = SharedConnection <$> newTMVarIO mempty

connectSharedDb :: SharedConnection -> ByteString -> IO Connection
connectSharedDb (SharedConnection v) connstr = do
  -- This part may retry if connection is underway
  trial <- atomically $ do
    m <- takeTMVar v
    case m !? connstr of
      Just conn -> do
        -- Put contents back and return pre-existing connection
        putTMVar v m
        pure $ Right conn
      Nothing -> do
        -- Leave var empty and go for reconnect
        pure $ Left m
  -- Either return previous or connect
  case trial of
    Right a -> pure a
    Left m -> let act = do
                    -- Connect and put back
                    conn <- connectPostgreSQL connstr
                    atomically $ putTMVar v $ insert connstr conn m
                    pure conn
                  err = atomically $ putTMVar v m
              in act `onException` err

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
