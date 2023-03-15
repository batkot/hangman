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
import Data.Foldable (foldl')
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

runGame :: Foldable f => Game 'Running -> f Char -> AnyGame
runGame game = foldl' step (AnyGame game)
  where
    step :: AnyGame -> Char -> AnyGame
    step (AnyGame (RunningGame a b c)) guess = guessLetter guess $ RunningGame a b c
    step x _ = x

getWrongChars :: InfiniteList Char -> Solution -> [Char]
getWrongChars infiniteChars (Solution solution) = wrongChars
  where
    puzzleChars = toUpper <$> nubOrdOn toUpper solution
    wrongChars = filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars

guessingAllPuzzleCharsSolvesThePuzzle :: RunningGameData 'Any -> Bool
guessingAllPuzzleCharsSolvesThePuzzle RunningGameData{..} =
    isWon result
  where
    guesses = nubOrdOn toUpper $ unSolution solution
    result = runGame game guesses

guessingWrongCharNTimesLosesTheGame :: RunningGameData 'Any -> InfiniteList Char -> Bool
guessingWrongCharNTimesLosesTheGame RunningGameData{..} infiniteChars =
   isLost result
  where
    chances = toInt . getLeftChances $ game
    result = runGame game $ take chances $ getWrongChars infiniteChars solution

guessingGuessedLetterDecreasesScore :: RunningGameData 'AtLeast2DifferentLetters -> Bool
guessingGuessedLetterDecreasesScore RunningGameData{..} =
    isLost result
  where
    existingChar = head . unSolution $ solution
    chances = toInt . getLeftChances $ game
    result = runGame game $ replicate (chances + 1) existingChar

canMakeSomeMistakesAndStillWin :: RunningGameData 'Any -> InfiniteList Char -> Bool
canMakeSomeMistakesAndStillWin RunningGameData{..} infiniteChars =
    isWon result
  where
    chances = toInt . getLeftChances $ game
    wrongGuesses = take (chances - 1) $ getWrongChars infiniteChars solution
    (lastGuess :| goodGuesses) = nubOrdOn toUpper $ unSolution solution
    guesses = concat . transpose $ [wrongGuesses, goodGuesses]
    result = runGame game $ guesses ++ [lastGuess]

data PuzzleKind = Any | AtLeast2DifferentLetters

data RunningGameData (state :: PuzzleKind) = RunningGameData
    { game :: Game 'Running
    , solution :: Solution
    }
    deriving stock (Eq, Show)

instance Arbitrary (RunningGameData 'Any) where
    arbitrary = do
        gameId <- GameId <$> arbitrary
        puzzle <- (:|) <$> arbitrary <*> arbitrary
        let solution = Solution puzzle
        chances <- arbitrary
        let game = createNewGame gameId solution chances
        return (RunningGameData game solution)

instance Arbitrary (RunningGameData 'AtLeast2DifferentLetters) where
    arbitrary = do
        gameId <- GameId <$> arbitrary
        puzzle <- (`suchThat` ((<) 1 . length . nubOrdOn toUpper)) $ (:|) <$> arbitrary <*> arbitrary
        chances <- arbitrary
        let solution = Solution puzzle
        let game = createNewGame gameId solution chances
        return (RunningGameData game solution)
