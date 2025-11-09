{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Be.Core.Registry
  ( discoverInstances
  , registerInstances
  , getInstances
  , lookupInstance
  )
where

import Data.Constraint (Class (..), Dict (..), (:-) (..))
import Data.HashMap.Strict qualified as HashMap
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import SomeDictOf (SomeDict, SomeDictOf (..))
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep, TypeRep, (:~~:) (..), eqTypeRep, someTypeRep, typeRep, typeRepTyCon, tyConName)

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

class SomeDictNC n k c | n -> k, c -> k where
  data SomeDictN n k c :: Type

instance k ~ Constraint => SomeDictNC 0 k c where
  data SomeDictN 0 k c where
    SomeDict0 :: c => SomeDictN 0 k c

instance (k ~ (k1 -> Constraint), SomeDictNC 1 k c) => SomeDictNC 1 k c where
  data SomeDictN 1 k c where
    SomeDict1 :: c a => Proxy a -> SomeDictN 1 k c

instance (k ~ (k1 -> k2 -> Constraint), SomeDictNC 2 k c) => SomeDictNC 2 k c where
  data SomeDictN 2 k c where
    SomeDict2 :: (c a b, x => c a b) => Proxy x -> Proxy a -> Proxy b -> SomeDictN 2 k c

w :: [SomeDictN _ _ HasCallStack]
w = [SomeDict0]

x :: [SomeDictN _ _ Ord]
x = [SomeDict1 (Proxy @Int)]

y :: [SomeDictN _ _ Monad]
y = [SomeDict1 (Proxy @IO)]

class Foo a b
class Foo a b => Bar a b
instance Foo Int Bool
instance Bar Int Bool

z :: [SomeDictN _ _ Bar]
z = [SomeDict2 (Proxy :: Proxy (Foo Int Bool, Eq Int)) (Proxy @Int) (Proxy @Bool)]

discoverInstances' :: forall (c :: _ -> Constraint). Typeable c => TH.Code TH.Q [SomeDictN _ _ c]
discoverInstances' = undefined

discoverInstances :: forall (c :: _ -> Constraint). Typeable c => TH.Code TH.Q [SomeDict c]
discoverInstances = TH.liftCode do
  let className = tyConName (typeRepTyCon (typeRep @c))
  -- TODO: Handle instances with multiple type parameters?
  instanceDecs <- TH.reifyInstances (TH.mkName className) [TH.VarT (TH.mkName "a")]
  let listTE :: [TH.TExp a] -> TH.TExp [a]
      listTE = TH.TExp . TH.ListE . map TH.unType
  dicts <- fmap listTE $ traverse decToDict instanceDecs
  TH.examineCode [|| concat $$(TH.liftCode $ pure dicts) ||]

decToDict :: forall k (c :: k -> Constraint). TH.InstanceDec -> TH.Q (TH.TExp [SomeDict c])
decToDict = \case
  TH.InstanceD _mOverlap cxt typ _decs ->
    if null cxt then do
      let stripSig = \case
            TH.SigT a _ -> a
            x -> x
      let t = case typ of
            TH.AppT _ t' -> stripSig t'
            _ -> t
      let proxy = [| Proxy :: Proxy $(pure t) |]
      TH.unsafeTExpCoerce [| [ SomeDictOf $proxy ] |]
    else
      -- TODO: Handle instances with context
      TH.examineCode [|| [] ||]
  _ -> do
    TH.reportWarning "discoverInstances called on 'reifyInstances' somehow returned something that wasn't a type class instance."
    TH.examineCode [|| [] ||]

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
