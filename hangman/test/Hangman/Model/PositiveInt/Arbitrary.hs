{-# OPTIONS_GHC -Wno-orphans #-}

module Hangman.Model.PositiveInt.Arbitrary () where

import Test.QuickCheck (Arbitrary(..))
import Hangman.Model.PositiveInt

instance Arbitrary PositiveInt where
  arbitrary = foldr add one . (`replicate` one) . abs <$> arbitrary
