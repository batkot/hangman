{-# OPTIONS_GHC -Wno-orphans #-}

module Hangman.Model.PositiveInt.Arbitrary () where

import           Hangman.Model.PositiveInt
import           Test.QuickCheck           (Arbitrary (..))

instance Arbitrary PositiveInt where
  arbitrary = foldr add one . (`replicate` one) . abs <$> arbitrary
