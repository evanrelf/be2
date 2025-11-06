{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

module Be.Value
  ( Value
  , SomeValue (..)
  , toSomeValue
  , fromSomeValue
  )
where

import Codec.Serialise (Serialise (..))
import Type.Reflection (TypeRep, eqTypeRep, typeRep, (:~~:) (..))

class (Typeable a, Show a, Serialise a, Hashable a) => Value a

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
  decode = undefined -- HARD PROBLEM

instance Value SomeValue

toSomeValue :: Value a => a -> SomeValue
toSomeValue x = SomeValue typeRep x

fromSomeValue :: Value a => SomeValue -> Maybe a
fromSomeValue @a (SomeValue t x) =
  case eqTypeRep t (typeRep @a) of
    Just HRefl -> Just x
    Nothing -> Nothing
