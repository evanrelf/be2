{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Build
  ( BuildSystem (..)
  , State (..)
  , newState
  , stateRealize
  , TaskContext (..)
  , taskContextRealize
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
  tasks :: TaskContext a -> Key a -> IO (Value a, Bool)

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
  taskContext <- atomically $ newTaskContext state
  (value, volatile) <- tasks taskContext key
  when (not volatile) do
    deps <- readTVarIO taskContext.deps
    _traceId <- insertTrace state.connection Trace{ key, deps, value }
    pure ()
  pure value

data TaskContext b = TaskContext
  { state :: State b
  , deps :: TVar (Map (Key b) Hash)
  }

newTaskContext :: State b -> STM (TaskContext b)
newTaskContext state = do
  deps <- newTVar Map.empty
  pure TaskContext{ state, deps }

taskContextRealize :: BuildSystem b => TaskContext b -> Key b -> IO (Value b)
taskContextRealize taskContext key = do
  value <- stateRealize taskContext.state key
  atomically $ modifyTVar' taskContext.deps $ Map.insert key (hash value)
  pure value
