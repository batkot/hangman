{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hangman.RulesSpec
    ( test_rules
    ) where

import Prelude hiding (head)
import Test.Tasty (TestTree, testGroup)

import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty ((:|)), head)
import Data.List.NonEmpty.Extra (nubOrdOn)
import Hangman.Rules
import Hangman.PositiveInt
import Data.Foldable (foldrM)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), InfiniteList(..), suchThat)
import Hangman.PositiveInt.Arbitrary ()
import GHC.Unicode (toUpper)

test_rules :: TestTree
test_rules = testGroup "Hangman.Rules tests"
    [ testProperty "Guessing all puzzle chars solves the puzzle" guessingAllPuzzleCharsSolvesThePuzzle
    , testProperty "Guessing wrong chars N times fails the game" guessingWrongCharNTimesLosesTheGame
    , testProperty "Guessing guessed letter decreases score" guessingGuessedLetterDecreasesScore
    , testProperty "Can make N - 1 mistakes and still win" canMakeSomeMistakesAndStillWin
    ]

getWrongChars :: InfiniteList Char -> NonEmpty Char -> [Char]
getWrongChars infiniteChars puzzle = wrongChars
  where
    puzzleChars = toUpper <$> nubOrdOn toUpper puzzle
    wrongChars = filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars

guessingAllPuzzleCharsSolvesThePuzzle :: RunningGameData 'Any -> Bool
guessingAllPuzzleCharsSolvesThePuzzle RunningGameData{..} =
    result == Left Victory
  where
    guesses = nubOrdOn toUpper puzzle
    result = foldrM guessLetter game guesses

guessingWrongCharNTimesLosesTheGame :: RunningGameData 'Any -> InfiniteList Char -> Bool
guessingWrongCharNTimesLosesTheGame RunningGameData{..} infiniteChars =
   result == Left Failure
  where
    chances = toInt . getLeftChances $ game
    result = foldrM guessLetter game $ take chances $ getWrongChars infiniteChars puzzle

guessingGuessedLetterDecreasesScore :: RunningGameData 'AtLeast2DifferentLetters -> Bool
guessingGuessedLetterDecreasesScore RunningGameData{..} =
    result == Left Failure
  where
    existingChar = head puzzle
    chances = toInt . getLeftChances $ game
    result = foldrM guessLetter game $ replicate (chances + 1) existingChar

canMakeSomeMistakesAndStillWin :: RunningGameData 'Any -> InfiniteList Char -> Bool
canMakeSomeMistakesAndStillWin RunningGameData{..} infiniteChars =
    result == Left Victory
  where
    chances = toInt . getLeftChances $ game
    wrongGuesses = take (chances - 1) $ getWrongChars infiniteChars puzzle
    (lastGuess :| goodGuesses) = nubOrdOn toUpper puzzle
    guesses = concat . transpose $ [wrongGuesses, goodGuesses]
    result = foldrM guessLetter game $ guesses ++ [lastGuess]

data PuzzleKind = Any | AtLeast2DifferentLetters

data RunningGameData (state :: PuzzleKind) = RunningGameData
    { game :: RunningGame
    , puzzle :: NonEmpty Char
    }
    deriving stock (Eq, Show)

instance Arbitrary (RunningGameData 'Any) where
    arbitrary = do
        puzzle <- (:|) <$> arbitrary <*> arbitrary
        chances <- arbitrary
        let game = createNewGame puzzle chances
        return (RunningGameData game puzzle)

instance Arbitrary (RunningGameData 'AtLeast2DifferentLetters) where
    arbitrary = do
        puzzle <- (`suchThat` ((<) 1 . length . nubOrdOn toUpper)) $ (:|) <$> arbitrary <*> arbitrary
        chances <- arbitrary
        let game = createNewGame puzzle chances
        return (RunningGameData game puzzle)
