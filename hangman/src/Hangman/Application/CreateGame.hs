{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.CreateGame
    ( Command(..)
    , createGame
    , createRandomGame) where

import Data.List.NonEmpty (NonEmpty)
import Hangman.Model.PositiveInt (PositiveInt)
import Hangman.Model.Game (createNewGame, RunningGame)

data Command = Command
    { puzzle :: NonEmpty Char
    , chances :: PositiveInt
    }

newtype GameId = GameId { getId :: String }

class Monad m => SaveGameMonad m where
    saveGame :: RunningGame -> m GameId

createGame
    :: SaveGameMonad m
    => Command
    -> m GameId
createGame Command{..} =
    saveGame $ createNewGame puzzle chances

class Monad m => GeneratePuzzleMonad m where
    generatePuzzle :: m (NonEmpty Char)

createRandomGame
    :: GeneratePuzzleMonad m
    => SaveGameMonad m
    => PositiveInt
    -> m GameId
createRandomGame chances = do
    puzzle <- generatePuzzle
    createGame $ Command {..}

