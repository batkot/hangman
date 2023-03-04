{-# OPTIONS_GHC -Wno-orphans #-}

module Hangman.PositiveInt.Arbitrary () where

import Test.QuickCheck (Arbitrary(..))
import Hangman.PositiveInt

instance Arbitrary PositiveInt where
  arbitrary = foldr add one . (`replicate` one) . abs <$> arbitrary
