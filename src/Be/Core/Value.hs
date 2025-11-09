{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}

module Be.Core.Value
  ( Value
  , SomeValue (..)
  , toSomeValue
  , fromSomeValue
  , fromSomeValue'
  )
where

import Be.Core.Registry (lookupInstance)
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Data.Constraint (Class (..), Dict (..), (:-) (..))
import SomeDictOf (SomeDictOf (..))
import Text.Show (showsPrec)
import Type.Reflection (TypeRep, (:~~:) (..), eqTypeRep, typeRep)

class (Typeable a, Show a, Serialise a, Hashable a) => Value a

instance Class (Typeable a) (Value a) where
  cls = Sub Dict

data SomeValue where
  SomeValue :: Value a => TypeRep a -> a -> SomeValue

instance Value SomeValue

instance Show SomeValue where
  -- TODO: Show an existential `Int` as `42 :: Int` rather than `42`?
  showsPrec precedence (SomeValue _ x) = showsPrec precedence x

instance Eq SomeValue where
  SomeValue t1 x1 == SomeValue t2 x2 =
    case eqTypeRep t1 t2 of
      Just HRefl -> x1 == x2
      Nothing -> False

instance Hashable SomeValue where
  -- TODO: Hash `show typeRep` rather than `typeRep` for more stable hashing?
  -- Otherwise `42 :: Int` and `42 :: Word64` serialize to the same CBOR and get
  -- the same hash.
  hashWithSalt salt (SomeValue _ x) = hashWithSalt salt x

instance Serialise SomeValue where
  encode (SomeValue t x) = encode (t, x)
  decode = do
    decodeListLenOf 2
    t <- decode
    case lookupInstance @Value t of
      Just (SomeDictOf (Proxy @a)) -> SomeValue (typeRep @a) <$> decode @a
      Nothing -> fail $ "Failed to deserialize value of type `" <> show t <> "`; `Value` instance missing from registry"

toSomeValue :: Value a => a -> SomeValue
toSomeValue x = SomeValue typeRep x

fromSomeValue :: Value a => SomeValue -> Maybe a
fromSomeValue @a (SomeValue t x) =
  case eqTypeRep t (typeRep @a) of
    Just HRefl -> Just x
    Nothing -> Nothing

fromSomeValue' :: Value a => SomeValue -> a
fromSomeValue' @a x@(SomeValue t _) =
  fromMaybe (error message) (fromSomeValue x)
  where
  message =
    unwords
      [ "fromSomeValue':"
      , "Expected `" <> show (typeRep @a) <> "`,"
      , "got `" <> show t <> "`"
      ]
