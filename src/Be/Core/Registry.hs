{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Global runtime registry for typeclass instances.
--
-- This module provides a way to register and look up typeclass instances at runtime.
-- This is necessary for dynamic dispatch in the build system - we need to:
--
-- 1. Deserialize SomeValue without knowing the concrete type at compile time
-- 2. Route task keys to their handlers without exhaustive pattern matching
--
-- The registry is implemented as a global IORef (using unsafePerformIO for top-level
-- initialization). Instances are discovered at compile time via Template Haskell
-- (from the discover-instances library) and registered at startup via 'initBuild'.
--
-- REVIEW: Using unsafePerformIO for global state is a classic Haskell pattern but
-- comes with risks. The NOINLINE pragmas are critical - without them, GHC might
-- inline the IORef creation, giving you multiple independent registries! The current
-- implementation is correct, but this is subtle.
--
-- REVIEW: The registry doesn't support removing instances. Once registered, they're
-- there forever. This is probably fine for most use cases (instances are typically
-- registered once at startup), but could be an issue for long-running processes that
-- dynamically load/unload code.
module Be.Core.Registry
  ( discoverInstances
  , registerInstances
  , getInstances
  , lookupInstance
  )
where

import Data.HashMap.Strict qualified as HashMap
import DiscoverInstances (Class (..), Dict (..), SomeDict, SomeDictOf (..), (:-) (..), discoverInstances)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep, TypeRep, (:~~:) (..), eqTypeRep, someTypeRep, typeRep)

type Registry = HashMap SomeTypeRep SomeInstances

type Instances c = HashMap SomeTypeRep (SomeDict c)

data SomeInstances where
  SomeInstances :: Typeable c => TypeRep c -> Instances c -> SomeInstances

toSomeInstances :: Typeable c => Instances c -> SomeInstances
toSomeInstances r = SomeInstances typeRep r

fromSomeInstances :: Typeable c => SomeInstances -> Maybe (Instances c)
fromSomeInstances @c (SomeInstances t r) =
  case eqTypeRep t (typeRep @c) of
    Just HRefl -> Just r
    Nothing -> Nothing

-- | Global registry of typeclass instances.
--
-- CRITICAL: The NOINLINE pragma is essential! Without it, GHC might create multiple
-- copies of the IORef, breaking the global singleton pattern.
registryIORef :: IORef Registry
registryIORef = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE registryIORef #-}

registerInstances
  :: (Typeable c, forall a. Class (Typeable a) (c a))
  => [SomeDict c] -> IO ()
registerInstances @c dicts = do
  let toEntry :: SomeDict c -> (SomeTypeRep, SomeDict c)
      toEntry dict@(SomeDictOf (proxy :: Proxy a)) =
        case cls @(Typeable a) @(c a) of
          Sub Dict -> (someTypeRep proxy, dict)
  let instances :: Instances c
      instances = HashMap.fromList $ map toEntry dicts
  alreadyRegistered <-
    atomicModifyIORef' registryIORef \registry ->
      ( HashMap.insert
          (someTypeRep (Proxy @c))
          (toSomeInstances instances)
          registry
      , HashMap.member (someTypeRep (Proxy @c)) registry
      )
  when alreadyRegistered do
    error $ "Instances for `" <> show (typeRep @c) <> "` already registered"

getInstancesIO :: Typeable c => IO (HashMap SomeTypeRep (SomeDict c))
getInstancesIO @c = do
  registry <- readIORef registryIORef
  case HashMap.lookup (someTypeRep (Proxy @c)) registry of
    Nothing ->
      error $ "Instances for `" <> show (typeRep @c) <> "` not yet registered"
    Just someInstances -> case fromSomeInstances someInstances of
      Nothing -> error "unreachable"
      Just instances -> pure instances

-- | Pure interface to instance lookup via unsafePerformIO.
--
-- This is safe because:
-- 1. The registry is only mutated during initialization (registerInstances)
-- 2. Once registered, instances are immutable
-- 3. HashMap.lookup is a pure operation
--
-- The NOINLINE pragma prevents GHC from duplicating the IO action.
getInstances :: Typeable c => HashMap SomeTypeRep (SomeDict c)
getInstances @c = unsafePerformIO (getInstancesIO @c)
{-# NOINLINE getInstances #-}

lookupInstanceIO :: Typeable c => SomeTypeRep -> IO (Maybe (SomeDict c))
lookupInstanceIO @c t = do
  instances <- getInstancesIO @c
  pure $ HashMap.lookup t instances

-- | Pure interface to instance lookup via unsafePerformIO.
--
-- Same safety reasoning as 'getInstances'.
lookupInstance :: Typeable c => SomeTypeRep -> Maybe (SomeDict c)
lookupInstance @c t = unsafePerformIO (lookupInstanceIO @c t)
{-# NOINLINE lookupInstance #-}
