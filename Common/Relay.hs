{-# LANGUAGE DeriveGeneric #-}
module Common.Relay where

import Data.Aeson
import GHC.Generics

import Common.ConfigHelpers (fieldMangler)

type RelayReader = IO (RelayState, Value)
type RelayWriter = Bool -> IO RelayResponse

data Relay = Relay
  { readRelay  :: RelayReader
  , writeRelay :: RelayWriter
  }

data RelayState = RelayState
  { relayState  :: Bool
  , relayMode   :: RelayMode
  } deriving (Generic, Show)

data RelayMode = RelayNormal
               | RelayManual
               | RelayBooted
               | RelayTimeout
               deriving(Generic, Show)

newtype RelayResponse = RelayResponse
  { oldState :: Maybe Bool
  } deriving (Show)

instance ToJSON RelayState where
  toEncoding = genericToEncoding defaultOptions{fieldLabelModifier = fieldMangler 5}

instance ToJSON RelayMode where
  toEncoding = genericToEncoding defaultOptions{constructorTagModifier = fieldMangler 5}
