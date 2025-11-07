{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Be.Task
  ( Task (..)
  , discoverTasks
  )
where

import Be.Value (Value)
import Data.HashMap.Strict qualified as HashMap
import DiscoverInstances (SomeDict, SomeDictOf (..), discoverInstances)
import Language.Haskell.TH qualified as TH
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep, someTypeRep)
import VarArgs ((:->:))

type Task :: Type -> Constraint
class
  ( Typeable a
  , Value (TaskKey a)
  , Value (TaskValue a)
  , Coercible (TaskKey a) (ArgsToKey (TaskArgs a))
  , Coercible (TaskValue a) (TaskResult a)
  ) => Task a where
  type TaskArgs a :: [Type]
  type TaskResult a :: Type
  data TaskKey a :: Type
  data TaskValue a :: Type
  taskRealize :: proxy a -> TaskArgs a :->: IO (TaskResult a)
  taskBuild :: proxy a -> TaskArgs a :->: IO (TaskResult a)

taskRegistry :: IORef (HashMap SomeTypeRep (SomeDict Task))
taskRegistry = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE taskRegistry #-}

discoverTasks :: TH.Code TH.Q (IO ())
discoverTasks = [|| do
    let dicts :: [SomeDict Task]
        dicts = $$discoverInstances
    let tasks = dicts
          & map (\dict@(SomeDictOf proxy) -> (someTypeRep proxy, dict))
          & HashMap.fromList
    atomicModifyIORef' taskRegistry \tr -> (HashMap.union tasks tr, ())
  ||]

lookupTask :: SomeTypeRep -> Maybe (SomeDict Task)
lookupTask t = unsafePerformIO do
  vr <- readIORef taskRegistry
  pure $ HashMap.lookup t vr
{-# NOINLINE lookupTask #-}

-- TODO: Need a value-level function to convert for use in `taskRealize`.
type ArgsToKey :: [Type] -> Type
type family ArgsToKey xs = r | r -> xs where
  ArgsToKey '[] = () -- Weird but I'll allow it for now
  ArgsToKey '[x1] = Identity x1 -- `Solo` is missing instances
  ArgsToKey '[x1, x2] = (x1, x2)
  ArgsToKey '[x1, x2, x3] = (x1, x2, x3)
  ArgsToKey '[x1, x2, x3, x4] = (x1, x2, x3, x4)
  -- Ideally no tasks have more than 4 arguments...
