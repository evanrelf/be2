module Be.Build
  (
  )
where

import Be.Trace (Key, Value)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as Sqlite
import Prelude hiding (State)

data State k v = State
  { connection :: Sqlite.Connection
  , done :: TVar (Map k (TMVar ()))
  , store :: TVar (Map k v)
  }

newState :: Sqlite.Connection -> IO (State k v)
newState connection = do
  done <- newTVarIO Map.empty
  store <- newTVarIO Map.empty
  pure State{ connection, done, store }

realize :: (Key k, Value v) => State k v -> k -> IO v
realize = undefined

fetch :: (Key k, Value v) => State k v -> k -> IO (Maybe v)
fetch = undefined

build :: (Key k, Value v) => State k v -> k -> IO v
build = undefined
