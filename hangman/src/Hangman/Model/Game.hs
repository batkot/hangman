{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Hangman.Model.Game
    ( guessLetter
    , createNewGame
    , Game(..)
    , GameState(..)
    , FinishedGame
    , Chances
    , getLeftChances
    , isWon
    , isLost
    ) where

import Hangman.Model.PositiveInt (PositiveInt, decrement)
import Hangman.Model.Puzzle (Puzzle, PuzzleState(..), Solution, createPuzzle)
import qualified Hangman.Model.Puzzle as Puzzle (guessLetter)

import Data.Bifunctor (first)
import Data.Either.Combinators (maybeToRight, isLeft, isRight)

type Chances = PositiveInt

data GameState = Running | Lost | Won

data Game (state :: GameState) where
    RunningGame :: Puzzle 'Unsolved -> PositiveInt -> Game 'Running
    LostGame :: Puzzle 'Unsolved -> Game 'Lost
    WonGame :: Puzzle 'Solved -> Game 'Won

deriving stock instance Eq (Game state)
deriving stock instance Show (Game state)

type FinishedGame = Either (Game 'Lost) (Game 'Won)

getLeftChances :: Game 'Running -> Chances
getLeftChances (RunningGame _ chances) = chances

isWon :: Either FinishedGame (Game 'Running) -> Bool
isWon = either isRight (const False)

isLost :: Either FinishedGame (Game 'Running) -> Bool
isLost = either isLeft (const False)

createNewGame :: Solution -> Chances -> Game 'Running
createNewGame solution = RunningGame gamePuzzle
  where
    gamePuzzle = createPuzzle solution

guessLetter :: Char -> Game 'Running -> Either FinishedGame (Game 'Running)
guessLetter x (RunningGame puzzle chances) = do
    unsolvedPuzzle <- first gameWon (Puzzle.guessLetter x puzzle)
    newChances <-
        if puzzle == unsolvedPuzzle
        then maybeToRight (Left (LostGame unsolvedPuzzle)) $ decrement chances
        else Right chances
    return $ RunningGame unsolvedPuzzle newChances
  where
    gameWon = Right . WonGame
