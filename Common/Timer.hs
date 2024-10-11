module Common.Timer
  ( Target
  , newTarget
  , pushTarget
  , isTargetReached
  , waitForTarget
  ) where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import System.Clock

newtype Target = Target TimeSpec deriving (Show)

epochToTimespec :: RealFrac a => a -> TimeSpec
epochToTimespec x = fromNanoSecs $ floor $ x * 10^9

-- |Push target (move it n seconds forward from previous target, not
-- current time.
pushTarget :: RealFrac a => a -> Target -> Target
pushTarget offset (Target target) = Target $ target + epochToTimespec offset

-- |Create a new target given number of second in the future
newTarget :: RealFrac a => a -> IO Target
newTarget offset = do
  now <- getTime MonotonicCoarse
  pure $ Target $ now + epochToTimespec offset

-- |True, if the target is reached (current time is later than target)
isTargetReached :: Target -> IO Bool
isTargetReached (Target target) = do
  now <- getTime MonotonicCoarse
  pure $ target < now

-- |Uses threadDelay to wait until the target.
waitForTarget :: Target -> IO ()
waitForTarget (Target target) = do
  now <- getTime MonotonicCoarse
  let diff = target - now
  when (diff > 0) $ threadDelay $ fromInteger $ toNanoSecs diff `div` 1000
