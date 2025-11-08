{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

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

getInstances :: Typeable c => HashMap SomeTypeRep (SomeDict c)
getInstances @c = unsafePerformIO (getInstancesIO @c)
{-# NOINLINE getInstances #-}

lookupInstanceIO :: Typeable c => SomeTypeRep -> IO (Maybe (SomeDict c))
lookupInstanceIO @c t = do
  instances <- getInstancesIO @c
  pure $ HashMap.lookup t instances

lookupInstance :: Typeable c => SomeTypeRep -> Maybe (SomeDict c)
lookupInstance @c t = unsafePerformIO (lookupInstanceIO @c t)
{-# NOINLINE lookupInstance #-}
