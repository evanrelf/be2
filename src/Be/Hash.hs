module Be.Hash
  ( Hash (..)
  , hash
  )
where

import Codec.Serialise (Serialise, serialise)
import Data.Digest.XXHash.FFI (XXH3 (..))
import Data.Hashable qualified as Hashable

newtype Hash = Hash Int
  deriving newtype (Serialise)

hash :: Serialise a => a -> Hash
hash x = Hash (Hashable.hash (XXH3 (serialise x)))
