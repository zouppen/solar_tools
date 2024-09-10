{-# LANGUAGE DeriveGeneric #-}
module Common.Relay where

import Data.Aeson
import GHC.Generics

type RelayReader = IO (RelayState, Value)
type RelayWriter = Bool -> IO RelayResponse

data Relay = Relay
  { readRelay  :: RelayReader
  , writeRelay :: RelayWriter
  }

data RelayState = RelayState
  { relayState  :: Bool
  , relayForced :: Bool
  } deriving (Generic, Show)

newtype RelayResponse = RelayResponse
  { oldState :: Maybe Bool
  } deriving (Show)

instance ToJSON RelayState where
  toEncoding = genericToEncoding defaultOptions
