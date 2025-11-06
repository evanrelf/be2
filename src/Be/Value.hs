{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}

module Be.Value
  ( Value
  , registerValues
  , SomeValue (..)
  , toSomeValue
  , fromSomeValue
  )
where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Data.HashMap.Strict qualified as HashMap
import DiscoverInstances (SomeDict, SomeDictOf (..), discoverInstances)
import Language.Haskell.TH qualified as TH
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (SomeTypeRep, TypeRep, eqTypeRep, someTypeRep, typeRep, (:~~:) (..))

class (Typeable a, Show a, Serialise a, Hashable a) => Value a where

valueRegistry :: IORef (HashMap SomeTypeRep (SomeDict Value))
valueRegistry = unsafePerformIO $ newIORef HashMap.empty
{-# NOINLINE valueRegistry #-}

registerValues :: TH.Code TH.Q (IO ())
registerValues = [|| do
    let dicts :: [SomeDict Value]
        dicts = $$discoverInstances
    let values = dicts
          & map (\dict@(SomeDictOf proxy) -> (someTypeRep proxy, dict))
          & HashMap.fromList
    atomicModifyIORef' valueRegistry \vr -> (HashMap.union values vr, ())
  ||]

lookupValue :: SomeTypeRep -> Maybe (SomeDict Value)
lookupValue t = unsafePerformIO do
  vr <- readIORef valueRegistry
  pure $ HashMap.lookup t vr
{-# NOINLINE lookupValue #-}

data SomeValue where
  SomeValue :: Value a => TypeRep a -> a -> SomeValue

deriving stock instance Show SomeValue

instance Eq SomeValue where
  SomeValue t1 x1 == SomeValue t2 x2 =
    case eqTypeRep t1 t2 of
      Just HRefl -> x1 == x2
      Nothing -> False

instance Hashable SomeValue where
  hashWithSalt salt (SomeValue t x) = hashWithSalt salt (t, x)

instance Serialise SomeValue where
  encode (SomeValue t x) = encode (t, x)
  decode = do
    decodeListLenOf 2
    t <- decode
    case lookupValue t of
      Just (SomeDictOf (Proxy @a)) ->
        SomeValue (typeRep @a) <$> decode @a
      Nothing ->
        fail "fuck"

instance Value SomeValue

toSomeValue :: Value a => a -> SomeValue
toSomeValue x = SomeValue typeRep x

fromSomeValue :: Value a => SomeValue -> Maybe a
fromSomeValue @a (SomeValue t x) =
  case eqTypeRep t (typeRep @a) of
    Just HRefl -> Just x
    Nothing -> Nothing
