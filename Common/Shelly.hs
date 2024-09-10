{-# LANGUAGE OverloadedStrings #-}
module Common.Shelly (initShelly) where

import Common.Relay
import Network.Curl.Aeson
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL

-- |Initializes Shelly.
initShelly :: Applicative f => String -> f Relay
initShelly url = pure $ Relay (readShellyStatus url) (setShellyRelay url)

-- |Reads Shelly relay state without decoding JSON
readShellyStatus :: String -> IO RelayState
readShellyStatus url = curlAesonRaw both mempty "POST" (url <> "/rpc/Switch.GetStatus") payload
  where payload = jsonPayload $ object ["id" .= (0::Int)]
        parser (Object o) = RelayState <$> o .: "output" <*> toRelayForced o
        parser _ = mempty
        toRelayForced o = f <$> o .: "source"
        f :: String -> Maybe Bool
        f "WS_in"  = Just True  -- Forced from Web UI
        f "button" = Just True  -- Forced by pressing a button on the device
        f _        = Just False
        both bs = do
          val <- eitherDecode bs
          proto <- parseEither parser val
          pure $ proto val

setShellyRelay :: String -> Bool -> IO BL.ByteString
setShellyRelay url x = curlAesonRaw pure mempty "POST" (url <> "/rpc/Switch.Set") payload
  where payload = jsonPayload $ object ["id" .= (0::Int), "on" .= x]
