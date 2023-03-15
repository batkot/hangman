{-# LANGUAGE RecordWildCards #-}

module Hangman.Model.Puzzle.Spec
    ( test_puzzle
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, InfiniteList (getInfiniteList))

import Data.Foldable (foldrM)
import Data.Bifunctor (first)
import Hangman.Model.Puzzle
import Hangman.Model.Puzzle.Arbitrary
import Data.List.NonEmpty.Extra (nubOrdOn)
import GHC.Unicode (toUpper)

test_puzzle :: TestTree
test_puzzle = testGroup "Hangman.Model.Puzzle tests"
    [ testProperty "Guessing wrong letter does not change the puzzle" guessingWrongLetterDoesntChangePuzzle
    , testProperty "Guessing same letter twice does not change the puzzle" guessingSameLetterTwiceDoesntChangePuzzle
    , testProperty "Guessing all solution letters solves the puzzle" guessingAllSolutionLettersSolvesThePuzzle
    ]

guessingWrongLetterDoesntChangePuzzle :: UnsolvedPuzzle -> InfiniteList Char -> Bool
guessingWrongLetterDoesntChangePuzzle UnsolvedPuzzle{..} infiniteChars =
    Right puzzle == guessLetter wrongGuess puzzle
  where
    puzzleChars = toUpper <$> nubOrdOn toUpper solution
    wrongGuess = head . filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars

guessingSameLetterTwiceDoesntChangePuzzle :: UnsolvedPuzzle -> Char -> Bool
guessingSameLetterTwiceDoesntChangePuzzle UnsolvedPuzzle{..} guess =
    singleGuess == secondGuess
  where
    singleGuess = guessLetter guess puzzle
    secondGuess = singleGuess >>= guessLetter guess

guessingAllSolutionLettersSolvesThePuzzle :: UnsolvedPuzzle -> Bool
guessingAllSolutionLettersSolvesThePuzzle UnsolvedPuzzle{..} =
    Left (toUpper <$> solution) == guessResult
  where
    guessResult = first getSolution $ foldrM guessLetter puzzle solution
