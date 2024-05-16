{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)
import Control.Monad (void)

data State = State { epoch       :: Scientific
                   , jouleSum    :: Scientific
                   } deriving (Show, Read)

data Stats = Stats { added   :: !Integer
                   , skipped :: !Integer
                   } deriving (Show)

data Config = Config { connString :: ByteString
                     , stateGet   :: Query
                     , stateSet   :: Query
                     , initialGet :: Query
                     , select     :: Query
                     , insert     :: Query
                     }

config = Config { connString = "dbname=sensor"
                , stateGet = "select cursor from cursor where source='joule_state'"
                , stateSet = "insert into cursor values ('joule_state', ?) on conflict (source) do update set cursor=excluded.cursor"
                , initialGet = "select extract(epoch from ts) from aurinko order by ts limit 1"
                , select =  "select id, extract(epoch from ts), solar_power from aurinko where ts>to_timestamp(?) order by ts"
                , insert = "insert into aurinko_joule (id, joule_cum) values (?, ?)"
                }

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
            one [e] = pure $ State e 0
          in singleQuery conn none one $ initialGet config
      one [a] = maybe stateFail pure (readMaybe a)
    in singleQuery conn none one $ stateGet config
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Connection -> State -> IO ()
storeState conn st = void $ execute conn (stateSet config) [show st]

-- |Integrate unprocessed data from database and folding it
integrate :: Connection -> State -> IO (State, Stats)
integrate conn st = fold conn (select config)
  [epoch st] (st, Stats 0 0) (integrator conn)

-- |Folding function which handles single input row.
integrator
  :: Connection
  -> (State, Stats)
  -> (Int32, Scientific, Scientific)
  -> IO (State, Stats)
integrator conn (State oldTime oldSum, Stats{..}) (id, newTime, power) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ (State newTime newSum, Stats added (skipped + 1))
    else do execute conn (insert config) (id, newRounded)
            pure (State newTime newSum, Stats (added + 1) skipped)
  where delta = newTime - oldTime
        joules = power * delta
        newSum = oldSum + joules
        newRounded = (round newSum) :: Int64

main :: IO ()
main = do
  conn <- connectPostgreSQL $ connString config
  withTransaction conn $ do
    state <- stateInit conn
    (newState, stats) <- integrate conn state
    storeState conn newState
    putStrLn $ "Finished. " ++ show stats
