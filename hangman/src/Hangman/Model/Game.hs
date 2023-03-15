{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Hangman.Model.Game
    ( guessLetter
    , createNewGame
    , Solution(..)
    , Game(..)
    , GameState(..)
    , GameId(..)
    , GameRepository(..)
    , AnyGame(..)
    , getLeftChances
    , isWon
    , isLost
    ) where

import Data.List.NonEmpty (NonEmpty)

import Hangman.Model.PositiveInt (PositiveInt, decrement)
import Hangman.Model.Puzzle (Puzzle, PuzzleState(..), createPuzzle)
import qualified Hangman.Model.Puzzle as Puzzle (guessLetter)

newtype Solution = Solution { unSolution :: NonEmpty Char } deriving newtype (Eq, Show)
type Chances = PositiveInt

getLeftChances :: Game 'Running -> Chances
getLeftChances (RunningGame _ _ chances) = chances

data GameState = Running | Lost | Won

newtype GameId = GameId { unGameId :: String }
    deriving newtype Eq
    deriving stock Show

data Game (state :: GameState) where
    RunningGame :: GameId -> Puzzle 'Unsolved -> PositiveInt -> Game 'Running
    LostGame :: GameId -> Puzzle 'Unsolved -> Game 'Lost
    WonGame :: GameId -> Puzzle 'Solved -> Game 'Won

deriving instance Eq (Game state)
deriving instance Show (Game state)

data AnyGame = forall (state :: GameState). AnyGame (Game state)

isWon :: AnyGame -> Bool
isWon (AnyGame (WonGame _ _)) = True
isWon _ = False

isLost :: AnyGame -> Bool
isLost (AnyGame (LostGame _ _)) = True
isLost _ = False

class Monad m => GameRepository m where
    findGame :: GameId -> m (Maybe (Game 'Running))
    saveGame :: AnyGame -> m ()

createNewGame :: GameId -> Solution -> Chances -> Game 'Running
createNewGame gameId (Solution solution) = RunningGame gameId gamePuzzle
  where
    gamePuzzle = createPuzzle solution

guessLetter :: Char -> Game 'Running -> AnyGame
guessLetter x (RunningGame gameId puzzle chances) =
    either gameWon gameOn $ Puzzle.guessLetter x puzzle
  where
    gameWon solvedPuzzle = AnyGame $ WonGame gameId solvedPuzzle
    gameOn newPuzzle = maybe (AnyGame (LostGame gameId newPuzzle)) AnyGame newGame
      where
        newChances = if newPuzzle == puzzle then decrement chances else Just chances
        newGame = RunningGame gameId newPuzzle <$> newChances
