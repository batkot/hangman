{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

module Hangman.Model.GameSpec
    ( test_rules
    ) where

import Prelude hiding (head)
import Test.Tasty (TestTree, testGroup)

import Data.List (transpose)
import Data.List.NonEmpty (NonEmpty ((:|)), head)
import Data.List.NonEmpty.Extra (nubOrdOn)
import Hangman.Model.Game
import Hangman.Model.PositiveInt
import Hangman.Model.PositiveInt.Arbitrary ()
import Hangman.Model.Puzzle (Solution)
import Data.Foldable (foldrM)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), InfiniteList(..), suchThat)
import GHC.Unicode (toUpper)

test_rules :: TestTree
test_rules = testGroup "Hangman.Model.Game tests"
    [ testProperty "Guessing all puzzle chars solves the puzzle" guessingAllPuzzleCharsSolvesThePuzzle
    , testProperty "Guessing wrong chars N times fails the game" guessingWrongCharNTimesLosesTheGame
    , testProperty "Guessing guessed letter decreases score" guessingGuessedLetterDecreasesScore
    , testProperty "Can make N - 1 mistakes and still win" canMakeSomeMistakesAndStillWin
    ]

runGame :: Foldable f => Game gameId 'Running -> f Char -> Either (FinishedGame gameId) (Game gameId 'Running)
runGame = foldrM guessLetter

getWrongChars :: InfiniteList Char -> Solution -> [Char]
getWrongChars infiniteChars solution = wrongChars
  where
    puzzleChars = toUpper <$> nubOrdOn toUpper solution
    wrongChars = filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars

guessingAllPuzzleCharsSolvesThePuzzle :: RunningGameData gameId 'Any -> Bool
guessingAllPuzzleCharsSolvesThePuzzle RunningGameData{..} =
    isWon result
  where
    guesses = nubOrdOn toUpper solution
    result = runGame game guesses

guessingWrongCharNTimesLosesTheGame :: RunningGameData gameId 'Any -> InfiniteList Char -> Bool
guessingWrongCharNTimesLosesTheGame RunningGameData{..} infiniteChars =
   isLost result
  where
    chances = toInt . getLeftChances $ game
    result = runGame game $ take chances $ getWrongChars infiniteChars solution

guessingGuessedLetterDecreasesScore :: RunningGameData gameId 'AtLeast2DifferentLetters -> Bool
guessingGuessedLetterDecreasesScore RunningGameData{..} =
    isLost result
  where
    existingChar = head solution
    chances = toInt . getLeftChances $ game
    result = runGame game $ replicate (chances + 1) existingChar

canMakeSomeMistakesAndStillWin :: RunningGameData gameId 'Any -> InfiniteList Char -> Bool
canMakeSomeMistakesAndStillWin RunningGameData{..} infiniteChars =
    isWon result
  where
    chances = toInt . getLeftChances $ game
    wrongGuesses = take (chances - 1) $ getWrongChars infiniteChars solution
    (lastGuess :| goodGuesses) = nubOrdOn toUpper solution
    guesses = concat . transpose $ [wrongGuesses, goodGuesses]
    result = runGame game $ guesses ++ [lastGuess]

data PuzzleKind = Any | AtLeast2DifferentLetters

data RunningGameData gameId (state :: PuzzleKind) = RunningGameData
    { game :: Game gameId 'Running
    , solution :: Solution
    }
    deriving stock (Eq, Show)

instance Arbitrary (RunningGameData gameId 'Any) where
    arbitrary = do
        solution <- (:|) <$> arbitrary <*> arbitrary
        chances <- arbitrary
        let game = createNewGame solution chances
        return (RunningGameData game solution)

instance Arbitrary (RunningGameData gameId 'AtLeast2DifferentLetters) where
    arbitrary = do
        solution <- (`suchThat` ((<) 1 . length . nubOrdOn toUpper)) $ (:|) <$> arbitrary <*> arbitrary
        chances <- arbitrary
        let game = createNewGame solution chances
        return (RunningGameData game solution)
