{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}

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
    , GameId(..)
    ) where

import           Hangman.Model.PositiveInt (PositiveInt, decrement)
import           Hangman.Model.Puzzle      (Puzzle, PuzzleState (..), Solution,
                                            createPuzzle)
import qualified Hangman.Model.Puzzle      as Puzzle (guessLetter)

import           Data.Bifunctor            (first)
import           Data.Either               (isLeft, isRight)
import           Data.Either.Extra         (maybeToEither)
import           Data.UUID
import           GHC.Generics              (Generic)

type Chances = PositiveInt

newtype GameId = GameId { unGameId :: UUID } deriving newtype (Eq,Show)

data GameState = Running | Lost | Won deriving stock (Eq, Show, Generic)

data Game gameId (state :: GameState) where
    RunningGame :: Puzzle 'Unsolved -> PositiveInt -> Game gameId 'Running
    LostGame :: Puzzle 'Unsolved -> Game gameId 'Lost
    WonGame :: Puzzle 'Solved -> Game gameId 'Won

deriving stock instance Eq (Game gameId state)
deriving stock instance Show (Game gameId state)

type FinishedGame gameId = Either (Game gameId 'Lost) (Game gameId 'Won)

getLeftChances :: Game gameId 'Running -> Chances
getLeftChances (RunningGame _ chances) = chances

isWon :: Either (FinishedGame gameId) (Game gameId 'Running) -> Bool
isWon = either isRight (const False)

isLost :: Either (FinishedGame gameId) (Game gameId 'Running) -> Bool
isLost = either isLeft (const False)

createNewGame :: Solution -> Chances -> Game gameId 'Running
createNewGame solution = RunningGame gamePuzzle
  where
    gamePuzzle = createPuzzle solution

guessLetter :: Char -> Game gameId 'Running -> Either (FinishedGame gameId) (Game gameId 'Running)
guessLetter x (RunningGame puzzle chances) = do
    unsolvedPuzzle <- first gameWon (Puzzle.guessLetter x puzzle)
    newChances <-
        if puzzle == unsolvedPuzzle
        then maybeToEither (Left (LostGame unsolvedPuzzle)) $ decrement chances
        else Right chances
    return $ RunningGame unsolvedPuzzle newChances
  where
    gameWon = Right . WonGame
