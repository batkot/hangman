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

newtype Solution = Solution { unSolution :: NonEmpty Char } deriving newtype (Eq, Show)
type Chances = PositiveInt

newtype Puzzle = Puzzle { unPuzzle :: NonEmpty PuzzleLetter } deriving newtype Eq
instance Show Puzzle where
  show (Puzzle x) = join $ toList $ intersperse " " $ fmap show x

isSolved :: Puzzle -> Bool
isSolved = all isGuessed . unPuzzle
  where
    isGuessed (Guessed _) = True
    isGuessed _ = False

getLeftChances :: Game 'Running -> Chances
getLeftChances (RunningGame _ _ chances) = chances

data GameState = Running | Lost | Won

newtype GameId = GameId { unGameId :: String }
    deriving newtype Eq
    deriving stock Show

data Game (state :: GameState) where
    RunningGame :: GameId -> Puzzle -> PositiveInt -> Game 'Running
    LostGame :: GameId -> Puzzle -> Game 'Lost
    WonGame :: GameId -> Puzzle -> Game 'Won

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
    gamePuzzle = Puzzle $ Hidden . toUpper <$> solution

guessLetter :: Char -> Game 'Running -> AnyGame
guessLetter x (RunningGame gameId puzzle chances)
    | isSolved newPuzzle = AnyGame (WonGame gameId newPuzzle)
    | otherwise = maybe (AnyGame (LostGame gameId newPuzzle)) AnyGame newGame
  where
    newPuzzle = Puzzle $ guess (toUpper x) <$> unPuzzle puzzle
    newChances = if newPuzzle == puzzle then decrement chances else Just chances
    newGame = RunningGame gameId newPuzzle <$> newChances
