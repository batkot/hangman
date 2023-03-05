{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hangman.Rules
    ( someFunc2
    , guessLetter
    , createNewGame
    , RunningGame
    , FinishedGame(..)
    , getLeftChances
    ) where

import Data.List.NonEmpty
import GHC.Unicode (toUpper)
import Hangman.PositiveInt (PositiveInt, decrement)
import Control.Monad (join)

data PuzzleLetter
    = Hidden Char
    | Guessed Char
    deriving stock Eq

instance Show PuzzleLetter where
  show (Hidden x) = '|' : x : "|"
  show (Guessed x) = [x]

guess :: Char -> PuzzleLetter -> PuzzleLetter
guess x (Hidden y)
    | x == y = Guessed y
    | otherwise = Hidden y
guess _ x = x

newtype Puzzle = Puzzle { unPuzzle :: NonEmpty PuzzleLetter } deriving newtype Eq

instance Show Puzzle where
  show (Puzzle x) = join $ toList $ intersperse " " $ fmap show x

isSolved :: Puzzle -> Bool
isSolved = all isGuessed . unPuzzle
  where
    isGuessed (Guessed _) = True
    isGuessed _ = False

data RunningGame = RunningGame
    { puzzle :: Puzzle
    , chances :: PositiveInt
    }
    deriving stock (Eq, Show)

getLeftChances :: RunningGame -> PositiveInt
getLeftChances = chances

data FinishedGame
    = Victory
    | Failure
    deriving stock (Eq, Show)

createNewGame :: NonEmpty Char -> PositiveInt -> RunningGame
createNewGame word = RunningGame gamePuzzle
  where
    gamePuzzle = Puzzle $ Hidden . toUpper <$> word

guessLetter :: Char -> RunningGame -> Either FinishedGame RunningGame
guessLetter x RunningGame{..}
    | isSolved newPuzzle = Left Victory
    | otherwise = maybe (Left Failure) Right newGame
  where
    newPuzzle = Puzzle $ guess (toUpper x) <$> unPuzzle puzzle
    newChances = if newPuzzle == puzzle then decrement chances else Just chances
    newGame = RunningGame newPuzzle <$> newChances

someFunc2 :: String
someFunc2 = "Hello from Hangman Lib"
