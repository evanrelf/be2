module Be.Hash
  ( Hash (..)
  , hash
  )
where

import ChibiHash (chibihash64)
import Codec.Serialise (Serialise, serialise)

newtype Hash = Hash Word64
  deriving stock (Show)
  deriving newtype (Eq, Ord, Serialise)

hash :: Serialise a => a -> Hash
hash x = Hash (chibihash64 (toStrict (serialise x)) 0)
