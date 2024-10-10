module Common.Timer
  ( Timer
  , newTimer
  , newInfiniteTimer
  , startTimer
  , isTimerRunning
  , isTimerRunningIO
  , atomically -- re-export for convenience
  ) where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM

newtype Timer = Timer (TVar Bool)

-- |Creates a timer which can be started (armed) and checked with STM.
newTimer :: RealFrac a => a -> IO Timer
newTimer timeout = do
  var <- newTVarIO False
  thread <- forkIO $ forever $ do
    -- Wait until another end has armed it
    atomically $ readTVar var >>= check
    -- It has been armed, start.
    threadDelay µs
    atomically $ writeTVar var False
  pure $ Timer var
  where µs = round $ 1000000 * timeout

-- |Fake timer which doesn't ever stop. Useful in cases where user
-- asks for no timeout.
newInfiniteTimer :: IO Timer
newInfiniteTimer = Timer <$> newTVarIO False

-- |Starts the timer.
startTimer :: Timer -> STM ()
startTimer (Timer var) = writeTVar var True

-- |Return timer state (=if timer is still running).
isTimerRunning :: Timer -> STM Bool
isTimerRunning (Timer var) = readTVar var

-- |Similar to isTimerRunning but is faster.
isTimerRunningIO :: Timer -> IO Bool
isTimerRunningIO (Timer var) = readTVarIO var
