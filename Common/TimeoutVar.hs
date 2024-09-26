{-# LANGUAGE RecordWildCards #-}
module Common.TimeoutVar
  ( RecurringTimeoutVar
  , newRecurringTimeoutVar
  , waitRecurringTimeout
  , stopRecurringTimeout
  ) where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.STM

data RecurringTimeoutVar = RecurringTimeoutVar
  { var :: TVar Bool
  , thread :: ThreadId
  }

-- |Creates a recurring timeout which means the timeout is calculated
-- from the end of the last waiting period, not wall clock. So it
-- doesn't tick every n seconds but every n seconds but is armed n
-- seconds after creation or last waitRecurringTimeout call.
newRecurringTimeoutVar :: RealFrac a => a -> IO RecurringTimeoutVar
newRecurringTimeoutVar timeout = do
  let timeoutMicro = round $ 10000000 * timeout
  var <- newTVarIO False
  thread <- forkIO $ forever $ do
    -- Wait until another end has read the signal
    atomically $ readTVar var >>= check . not
    -- Mark the timoout after given time has elapsed
    threadDelay timeoutMicro
    atomically $ writeTVar var True
  pure RecurringTimeoutVar{..}

-- |Wait until timeout. Retries only if timeout hasn't already
-- expired. Rearms the timer.
waitRecurringTimeout :: RecurringTimeoutVar -> STM ()
waitRecurringTimeout RecurringTimeoutVar{..} = do
  readTVar var >>= check
  writeTVar var False

stopRecurringTimeout :: RecurringTimeoutVar -> IO ()
stopRecurringTimeout RecurringTimeoutVar{..} = killThread thread
