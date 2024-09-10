{-# LANGUAGE DeriveGeneric #-}
module Common.Relay where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics

type RelayReader = IO (RelayState, Value)
type RelayWriter = Bool -> IO BL.ByteString

data Relay = Relay
  { readRelay  :: RelayReader
  , writeRelay :: RelayWriter
  }

data RelayState = RelayState
  { relayState  :: Bool
  , relayForced :: Bool
  } deriving (Generic, Show)

instance ToJSON RelayState where
  toEncoding = genericToEncoding defaultOptions

printBL :: BL.ByteString -> IO ()
printBL = BL.putStrLn
