{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.CreateGame
    ( createGame
    , createRandomGame
    ) where

import qualified Hangman.Model.Game as Game
import Hangman.Model.Puzzle (Solution)
import Hangman.Application.Ports (GameMonad(..), PuzzleGeneratorMonad (nextPuzzle))

data Command = Command
    { solution :: Solution
    , chances :: Game.Chances
    } deriving stock (Eq,Show)

createGame :: GameMonad m => Command -> m ()
createGame Command{..} = setGame $ Game.createNewGame solution chances

createRandomGame
    :: PuzzleGeneratorMonad m
    => GameMonad m
    => Game.Chances
    -> m ()
createRandomGame chances = do
    solution <- nextPuzzle
    createGame Command{..}

