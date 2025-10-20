module Be.Trace
  ( Trace (..)
  )
where

data Trace k v = Trace
  { key :: k
  , deps :: HashMap k Int
  , value :: v
  }
