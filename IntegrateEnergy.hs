{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)
import Control.Monad (void)

data State = State { epoch       :: Scientific
                   , jouleSum    :: Scientific
                   , addedRows   :: Integer -- Just stats
                   , droppedRows :: Integer -- Just stats
                   } deriving (Show, Read)

-- |Helper to handle getting initial values, containing only one single answer row
singleQuery :: FromRow a => Connection -> IO b -> (a -> IO b) -> Query -> IO b
singleQuery conn whenNone whenOne q = do
  ans <- query_ conn q
  case ans of
    []  -> whenNone
    [a] -> whenOne a
    _   -> fail $ "More than one answer to this query: " ++ show q

-- |Load state from db or generate an initial one
stateInit :: Connection -> IO State
stateInit conn = do
  let none = do
        let none = fail "No data in 'aurinko' table"
            one [e] = pure $ State e 0 0 0
          in singleQuery conn none one "select extract(epoch from ts) from aurinko order by ts limit 1"
      one [a] = maybe (fail "Invalid state format, consider running DELETE FROM cursor WHERE source='joule_state';") pure $ readMaybe a
    in singleQuery conn none one "select cursor from cursor where source='joule_state'"

-- |Stores the state to the database
storeState :: Connection -> State -> IO ()
storeState conn st = void $ execute conn "insert into cursor values ('joule_state', ?) on conflict (source) do update set cursor=excluded.cursor" [show st]

-- |Integrate energy by unprocessed data from database and folding it
integrateEnergy conn st = fold conn
  "select id, extract(epoch from ts), solar_power from aurinko where ts>to_timestamp(?) order by ts"
  [epoch st] st (integrator conn)

-- |Folding function which integrates the energy
integrator :: Connection -> State -> (Int32, Scientific, Scientific) -> IO State
integrator conn (State oldTime oldSum !addedRows !droppedRows) (id, newTime, power) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ State newTime newSum addedRows (droppedRows + 1)
    else do execute conn "insert into aurinko_joule (id, joule_cum) values (?, ?)" (id, newRounded)
            pure $ State newTime newSum (addedRows + 1) droppedRows
  where delta = newTime - oldTime
        joules = power * delta
        newSum = oldSum + joules
        newRounded = (round newSum) :: Int64

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=sensor"
  withTransaction conn $ do
    st <- stateInit conn
    newst <- integrateEnergy conn st
    storeState conn newst
    putStrLn $ "Finished. End state: " ++ show newst
