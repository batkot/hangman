{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}

module Hangman.Model.Puzzle.Arbitrary (UnsolvedPuzzle(..)) where

import Hangman.Model.Puzzle
import Test.QuickCheck (Arbitrary(arbitrary))
import Data.List.NonEmpty (NonEmpty(..))

data UnsolvedPuzzle = UnsolvedPuzzle
    { puzzle :: Puzzle 'Unsolved
    , solution :: Solution
    } deriving stock (Eq, Show)

instance Arbitrary UnsolvedPuzzle where
    arbitrary = do
        solution <- (:|) <$> arbitrary <*> arbitrary
        let puzzle = createPuzzle solution
        return $ UnsolvedPuzzle {..}

