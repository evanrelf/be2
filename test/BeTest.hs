{-# OPTIONS_GHC -Wno-unused-imports #-}

module BeTest where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (bool)
import Test.Tasty.HUnit

hprop_or :: Property
hprop_or = property do
  bool <- forAll Gen.bool
  (bool || True) === True
  (True || bool) === True

unit_true :: Assertion
unit_true = do
  True @?= True
