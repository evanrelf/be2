{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

module Be.Core.Registry
  ( discoverInstances
  , registerInstances
  , lookupInstanceIO
  , unsafeLookupInstance
  , getInstancesIO
  , unsafeGetInstances
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

-- TODO: Throw error if register is called more than once, or if lookup is
-- called before register. Either can break referential transparency.

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
  atomicModifyIORef' registryIORef \registry ->
    ( HashMap.insert
        (someTypeRep (Proxy @c))
        (toSomeInstances instances)
        registry
    , ()
    )

lookupInstanceIO :: Typeable c => SomeTypeRep -> IO (Maybe (SomeDict c))
lookupInstanceIO @c t = do
  registry <- readIORef registryIORef
  pure do
    someInstances <- HashMap.lookup (someTypeRep (Proxy @c)) registry
    instances <- fromSomeInstances someInstances
    HashMap.lookup t instances

unsafeLookupInstance :: Typeable c => SomeTypeRep -> Maybe (SomeDict c)
unsafeLookupInstance @c t = unsafePerformIO (lookupInstanceIO @c t)
{-# NOINLINE unsafeLookupInstance #-}

getInstancesIO :: Typeable c => IO (Maybe (HashMap SomeTypeRep (SomeDict c)))
getInstancesIO @c = do
  registry <- readIORef registryIORef
  pure do
    someInstances <- HashMap.lookup (someTypeRep (Proxy @c)) registry
    fromSomeInstances someInstances

unsafeGetInstances :: Typeable c => Maybe (HashMap SomeTypeRep (SomeDict c))
unsafeGetInstances @c = unsafePerformIO (getInstancesIO @c)
{-# NOINLINE unsafeGetInstances #-}
