{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}

import System.Environment (getArgs)

import qualified Data.Yaml as Y
import Data.Aeson
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)
import Text.Read (readMaybe)
import Control.Monad (void)
import Data.String (IsString, fromString)

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
                     } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    let str k = fromString <$> v .: k
    in Config
       <$> str "connString"
       <*> str "stateGet"
       <*> str "stateSet"
       <*> str "initialGet"
       <*> str "select"
       <*> str "insert"

-- |Helper to handle getting initial values, containing only one single answer row
singleQuery :: FromRow a => Connection -> IO b -> (a -> IO b) -> Query -> IO b
singleQuery conn whenNone whenOne q = do
  ans <- query_ conn q
  case ans of
    []  -> whenNone
    [a] -> whenOne a
    _   -> fail $ "More than one answer to this query: " ++ show q

-- |Load state from db or generate an initial one
stateInit :: Config -> Connection -> IO State
stateInit Config{..} conn = do
  let none = do
        let none = fail "No data in 'aurinko' table"
            one [e] = pure $ State e 0
          in singleQuery conn none one initialGet
      one [a] = maybe stateFail pure (readMaybe a)
    in singleQuery conn none one stateGet
  where stateFail = fail "Invalid state format, consider dropping state, \
                         \truncating table and repopulating everything"

-- |Stores the state to the database
storeState :: Config -> Connection -> State -> IO ()
storeState Config{..} conn st = void $ execute conn stateSet [show st]

-- |Integrate unprocessed data from database and folding it
integrate :: Config -> Connection -> State -> IO (State, Stats)
integrate conf@Config{..} conn st = fold conn select
  [epoch st] (st, Stats 0 0) (integrator conf conn)

-- |Folding function which handles single input row.
integrator
  :: Config
  -> Connection
  -> (State, Stats)
  -> (Int32, Scientific, Scientific)
  -> IO (State, Stats)
integrator Config{..} conn (State oldTime oldSum, Stats{..}) (id, newTime, power) = do
  -- Inserting data. Do not insert if it didn't increment
  if round oldSum == newRounded
    then pure $ (State newTime newSum, Stats added (skipped + 1))
    else do execute conn insert (id, newRounded)
            pure (State newTime newSum, Stats (added + 1) skipped)
  where delta = newTime - oldTime
        joules = power * delta
        newSum = oldSum + joules
        newRounded = (round newSum) :: Int64

main :: IO ()
main = do
  args <- getArgs
  conf <- case args of
    [confPath] -> do
      parsed <- Y.decodeFileEither confPath
      case parsed of
        Left e -> fail $ "Parse error in configuration file: " ++ show e
        Right a -> pure a
    _ -> fail $ "Give configuration file as the only argument"
  conn <- connectPostgreSQL $ connString conf
  withTransaction conn $ do
    state <- stateInit conf conn
    (newState, stats) <- integrate conf conn state
    storeState conf conn newState
    putStrLn $ "Finished. " ++ show stats
