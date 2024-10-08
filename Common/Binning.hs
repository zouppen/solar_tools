module Common.Binning ( toBin
                      ) where

import Data.Bits (xor, countLeadingZeros)
import Data.Word (Word64)

-- |Returns the most significant bit which flips between two numbers,
-- in range from -20 to 44. In case of a timestamp the range is from
-- about 1 microsecond to 500k years. Range overflows are relatively
-- safe, though, returning the max (44) if the difference is less
-- 2^43, undefined otherwise. Useful in binning cumulative values so
-- that data can be fetched at given granularity in Grafana, for
-- instance.
toBin :: RealFrac a => a -> a -> Int
toBin a b = 64 - point - countLeadingZeros changes
  where scale x = (floor (2^point * x)) :: Word64
        changes = xor (scale a) (scale b)
        point = 20
