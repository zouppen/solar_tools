{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Common.Shelly (initShelly) where

import Common.Relay
import Network.Curl.Aeson
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)

-- |Initializes Shelly.
initShelly :: Applicative f => String -> f Relay
initShelly url = pure $ Relay (readShellyStatus url) (setShellyRelay url)

-- |Reads Shelly relay state, getting important parts and then the raw
-- payload as a tuple.
readShellyStatus :: String -> IO (RelayState, Value)
readShellyStatus url = curlAesonCustomWith both mempty "POST" (url <> "/rpc/Switch.GetStatus") payload
  where payload = Just $ object ["id" .= (0::Int)]
        parser (Object o) = RelayState <$> o .: "output" <*> toMode o
        parser _ = mempty
        toMode o = sourceToMode <$> o .: "source"
        both val = (,val) <$> parser val

sourceToMode :: Text -> RelayMode
sourceToMode x = case x of
  "WS_in"  -> RelayManual -- Set from the Web UI
  "button" -> RelayManual -- Set by pressing a button on the device
  "init"   -> RelayBooted
  _        -> RelayNormal

-- |Sets Shelly relay state, returning old state.
setShellyRelay :: String -> Bool -> IO RelayResponse
setShellyRelay url x = curlAesonCustomWith p mempty "POST" (url <> "/rpc/Switch.Set") payload
  where payload = Just $ object ["id" .= (0::Int), "on" .= x]
        p (Object o) = RelayResponse <$> (Just <$> o .: "was_on")
        p _ = mempty
