{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module Hangman.Model.Puzzle.Arbitrary (UnsolvedPuzzle(..)) where

import           Data.List.NonEmpty   (NonEmpty (..))
import           Hangman.Model.Puzzle
import           Test.QuickCheck      (Arbitrary (arbitrary))

data UnsolvedPuzzle = UnsolvedPuzzle
    { puzzle   :: Puzzle 'Unsolved
    , solution :: Solution
    } deriving stock (Eq, Show)

instance Arbitrary UnsolvedPuzzle where
    arbitrary = do
        solution <- (:|) <$> arbitrary <*> arbitrary
        let puzzle = createPuzzle solution
        return $ UnsolvedPuzzle {..}

