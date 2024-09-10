{-# LANGUAGE OverloadedStrings #-}
module Common.Shelly (initShelly) where

import Common.Relay
import Network.Curl.Aeson
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

-- |Initializes Shelly.
initShelly :: Applicative f => String -> f Relay
initShelly url = pure $ Relay (readShellyStatus url) (setShellyRelay url)

-- |Reads Shelly relay state without decoding JSON
readShellyStatus :: String -> IO (RelayState, Value)
readShellyStatus url = curlAesonCustomWith both mempty "POST" (url <> "/rpc/Switch.GetStatus") payload
  where payload = Just $ object ["id" .= (0::Int)]
        parser (Object o) = RelayState <$> o .: "output" <*> toRelayForced o
        parser _ = mempty
        toRelayForced o = isForced <$> o .: "source"
        both val = do
          a <- parser val
          pure (a, val)

isForced :: Text -> Bool
isForced x = case x of
  "WS_in"  -> True  -- Forced from Web UI
  "button" -> True  -- Forced by pressing a button on the device
  _        -> False

setShellyRelay :: String -> Bool -> IO BL.ByteString
setShellyRelay url x = curlAesonRaw pure mempty "POST" (url <> "/rpc/Switch.Set") payload
  where payload = jsonPayload $ object ["id" .= (0::Int), "on" .= x]
