{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}

-- | Existential wrapper for heterogeneous build values with runtime type information.
--
-- This module provides the foundation for storing and retrieving values of different
-- types in the same data structures (HashMap, SQLite, etc.). Key features:
--
-- * 'Value' constraint: Combines Typeable, Show, Serialise, Hashable
-- * 'SomeValue': Existential type carrying a TypeRep alongside the value
-- * Type-safe serialization/deserialization with runtime type checking
-- * Integration with the instance registry for deserializing unknown types
--
-- The design ensures that values can be stored in a type-erased form (SomeValue)
-- but recovered safely with runtime type checks.
--
-- REVIEW: This is a textbook example of using existential types well! The TypeRep
-- tagging ensures type safety even through serialization boundaries.
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
import DiscoverInstances (Class (..), Dict (..), SomeDictOf (..), (:-) (..))
import Type.Reflection (TypeRep, (:~~:) (..), eqTypeRep, typeRep)
import Text.Show (showsPrec)

class (Typeable a, Show a, Serialise a, Hashable a) => Value a

instance Class (Typeable a) (Value a) where
  cls = Sub Dict

-- | Existential wrapper for any Value type, carrying runtime type information.
--
-- The TypeRep ensures we can:
-- 1. Check type equality at runtime (for safe unwrapping)
-- 2. Serialize the type information alongside the value
-- 3. Display meaningful error messages when types don't match
--
-- REVIEW: The Show instance displays the inner value without the type. This is
-- nice for debugging, but could be confusing when you see the same value printed
-- from different type contexts. Consider adding the type to the output, e.g.:
-- "42 :: Int" instead of just "42".
--
-- REVIEW: The Hashable instance only hashes the value, not the TypeRep. This
-- means SomeValue Int 42 and SomeValue Word64 42 would have the same hash (though
-- they compare unequal). This is probably fine for HashMap performance, but worth
-- noting as a potential source of collisions.
data SomeValue where
  SomeValue :: Value a => TypeRep a -> a -> SomeValue

instance Value SomeValue

instance Show SomeValue where
  showsPrec precedence (SomeValue _ x) = showsPrec precedence x

instance Eq SomeValue where
  SomeValue t1 x1 == SomeValue t2 x2 =
    case eqTypeRep t1 t2 of
      Just HRefl -> x1 == x2
      Nothing -> False

instance Hashable SomeValue where
  hashWithSalt salt (SomeValue _ x) = hashWithSalt salt x

-- | Serialize SomeValue by encoding both the TypeRep and the value.
--
-- Deserialization is more complex:
-- 1. Decode the TypeRep to learn what type we're deserializing
-- 2. Look up the Value instance for that type in the runtime registry
-- 3. Use the instance's Serialise constraint to decode the value
-- 4. Wrap it back up in SomeValue
--
-- This requires that all Value instances are registered via 'initBuild' before
-- deserialization occurs. If the type isn't registered, we fail with a helpful error.
--
-- REVIEW: The error message is clear and actionable. However, this is a runtime
-- failure - if you forget to call initBuild or register a type, you won't find out
-- until you try to deserialize. Consider adding compile-time checks if possible.
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
