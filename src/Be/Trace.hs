module Be.Trace
  ( Trace (..)
  )
where

import Be.Hash (Hash)
import Codec.Serialise (Serialise)

data Trace k v = Trace
  { key :: k
  , deps :: Map k Hash
  , value :: v
  }
  deriving stock (Generic)
  deriving anyclass (Serialise)
