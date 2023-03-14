{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Hangman.Model.Game
    ( guessLetter
    , createNewGame
    , RunningGame
    , FinishedGame(..)
    , getLeftChances
    ) where

import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty, toList, intersperse)
import GHC.Unicode (toUpper)
import Hangman.Model.PositiveInt (PositiveInt, decrement)

data PuzzleLetter
    = Hidden Char
    | Guessed Char
    deriving stock Eq

instance Show PuzzleLetter where
  show (Hidden x) = ['|', x, '|']
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

data GameState = Running | Finished

newtype GameId = GameId { unGameId :: String }
    deriving newtype Eq
    deriving stock Show

data Game (state :: GameState) where
    RunningGame' :: GameId -> Puzzle -> PositiveInt -> Game 'Running
    FinishedGame' :: GameId -> Puzzle -> Game 'Finished

data AnyGame = forall state. AnyGame (Game state)

class Monad m => GameRepository m where
    findGame :: GameId -> m (Maybe (Game 'Running))
    saveGame :: m AnyGame

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
