{-# LANGUAGE OverloadedStrings, TupleSections #-}
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
readShellyStatus :: String -> IO (BL.ByteString, Bool)
readShellyStatus url = curlAesonRaw (rawAndParsed parser) mempty "POST" (url <> "/rpc/Switch.GetStatus") payload
  where payload = jsonPayload $ object ["id" .= (0::Int)]
        parser (Object o) = o .: "output"
        parser _ = mempty

setShellyRelay :: String -> Bool -> IO BL.ByteString
setShellyRelay url x = curlAesonRaw pure mempty "POST" (url <> "/rpc/Switch.Set") payload
  where payload = jsonPayload $ object ["id" .= (0::Int), "on" .= x]

-- |Parser returning the parser input in addition to the output.
rawAndParsed :: FromJSON a => (a -> Parser b) -> BL.ByteString -> Either String (BL.ByteString, b)
rawAndParsed p bs = (bs,) <$> (eitherDecode bs >>= parseEither p)
