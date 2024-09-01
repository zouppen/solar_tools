-- |Implements type which is unparsed. The Any type is intentionally
-- opaque to prevent SQL injections. It's primary purpose is to get a
-- value which is used for future inserts without knowing anything
-- about its type.
module Common.Any (Any) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.ByteString.Builder (byteString)

newtype Any = Any ByteString

instance FromField Any where
  fromField f Nothing = returnError ConversionFailed f ""
  fromField _ (Just bs) = pure $ Any bs

instance ToField Any where
  toField (Any bs) = Plain (byteString bs)
