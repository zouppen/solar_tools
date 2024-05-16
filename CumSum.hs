{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.FromRow as P
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)

data Entry = Entry { id        :: Int32
                   , epoch     :: Scientific
                   , joule_cum :: Int64
                   , residual  :: Scientific
                   } deriving (Show, Read)

singleQuery :: P.FromRow a => P.Connection -> IO b -> (a -> IO b) -> P.Query -> IO b
singleQuery conn whenNone whenOne q = do
  ans <- P.query_ conn q
  case ans of
    []  -> whenNone
    [a] -> whenOne a
    _   -> fail $ "More than one answer to this query: " ++ show q

stateInit :: P.Connection -> IO Entry
stateInit conn = do
  let none = do
        let none = fail "No data in 'aurinko' table"
            one (i, e) = pure (Entry i e 0 0)
          in singleQuery conn none one "select id, extract(epoch from ts) from aurinko order by ts limit 1"
      one (P.Only a) = maybe (fail "Invalid state format, clean state") pure $ readMaybe a
    in singleQuery conn none one "select cursor from cursor where source='joule_state'"

storeState conn st = do
  P.execute conn "insert into cursor values ('joule_state', ?) on conflict (source) do update set cursor=excluded.cursor" [show st]
  pure ()

hello :: IO Entry
hello = do
  conn <- P.connectPostgreSQL "dbname=sensor"
  P.withTransaction conn $ do
    st <- stateInit conn
    storeState conn st
    -- debug
    pure st
