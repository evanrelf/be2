{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Build
  ( BuildSystem (..)
  , TaskContext (..)
  , realize
  )
where

import Be.Hash (Hash, hash)
import Be.Trace (IsKey, IsValue, Trace (..), fetchTraces, insertTrace)
import Control.Exception (assert)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple qualified as Sqlite
import Prelude hiding (State)

class (IsKey (Key a), IsValue (Value a)) => BuildSystem a where
  type Key a :: Type
  type Value a :: Type
  tasks :: Key a -> IO (Value a, Bool)

data State b = State
  { connection :: Sqlite.Connection
  , done :: TVar (Map (Key b) (TMVar ()))
  , store :: TVar (Map (Key b) (Value b))
  }

newState :: BuildSystem b => Sqlite.Connection -> STM (State b)
newState connection = do
  done <- newTVar Map.empty
  store <- newTVar Map.empty
  pure State{ connection, done, store }

stateRealize :: BuildSystem b => State b -> Key b -> IO (Value b)
stateRealize state key = do
  undefined

stateFetch :: BuildSystem b => State b -> Key b -> IO (Maybe (Value b))
stateFetch state key = do
  undefined
  -- traces <- fetchTraces state.connection (Just key)
  -- matches <-
  --   forM traces \trace -> do
  --     assert (hash trace.key == hash key) $ pure ()
  --     undefined
  -- undefined

stateBuild :: forall b. BuildSystem b => State b -> Key b -> IO (Value b)
stateBuild state key = do
  (value, volatile) <- tasks @b key
  when (not volatile) do
    let deps = undefined
    _traceId <- insertTrace state.connection Trace{ key, deps, value }
    pure ()
  pure value

data TaskContext b = TaskContext
  { state :: State b
  , deps :: TVar (Map (Key b) Hash)
  }

realize :: BuildSystem b => TaskContext b -> Key b -> IO (Value b)
realize taskContext key = do
  value <- stateRealize taskContext.state key
  atomically $ modifyTVar' taskContext.deps $ Map.insert key (hash value)
  pure value
