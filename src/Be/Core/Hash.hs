{-# LANGUAGE UndecidableInstances #-}

module Be.Core.Hash
  ( BeHash (..)
  , BeHashable (..)
  )
where

import ChibiHash (chibihash64)
import Codec.Serialise (Serialise, serialise)

newtype BeHash = BeHash Word64
  deriving stock (Show)
  deriving newtype (Eq, Serialise)

class BeHashable a where
  beHash :: a -> BeHash

instance {-# OVERLAPPABLE #-} Serialise a => BeHashable a where
  beHash :: a -> BeHash
  beHash x = BeHash (chibihash64 (toStrict (serialise x)) 0)
