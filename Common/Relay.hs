module Common.Relay where

import qualified Data.ByteString.Lazy.Char8 as BL

type RelayReader = IO (BL.ByteString, Bool)
type RelayWriter = Bool -> IO BL.ByteString

data Relay = Relay { readRelay  :: RelayReader
                   , writeRelay :: RelayWriter
                   }

printRelayData :: BL.ByteString -> IO ()
printRelayData = BL.putStrLn
