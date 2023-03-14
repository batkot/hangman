{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Hangman.Application.CreateGame
    ( Command(..)
    , createGame
    , createRandomGame) where

import Data.List.NonEmpty (NonEmpty)
import Hangman.Model.PositiveInt (PositiveInt)
import Hangman.Model.Game (createNewGame, GameId(..), GameRepository(..), Solution(..), AnyGame(..))

data Command = Command
    { gameId :: GameId
    , puzzle :: NonEmpty Char
    , chances :: PositiveInt
    }

createGame
    :: GameRepository m
    => Command
    -> m ()
createGame Command{..} =
    saveGame . AnyGame $ createNewGame gameId (Solution puzzle) chances

class Monad m => GeneratePuzzleMonad m where
    generatePuzzle :: m (NonEmpty Char)

createRandomGame
    :: GeneratePuzzleMonad m
    => GameRepository m
    => PositiveInt
    -> GameId
    -> m ()
createRandomGame chances gameId = do
    puzzle <- generatePuzzle
    createGame $ Command {..}

