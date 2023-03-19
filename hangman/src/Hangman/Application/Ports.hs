{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hangman.Application.Ports
    ( GameMonad(..)
    , PuzzleGeneratorMonad(..)
    ) where

import Hangman.Model.Game (Game, GameState(..), GameId)
import Hangman.Model.Puzzle (Solution)
import Hangman.Named (Named)

class Monad m => GameMonad m where
    getGame :: Named gameId GameId -> m (Game gameId 'Running)
    setGame :: Named gameId GameId -> Game gameId state -> m ()

class Monad m => PuzzleGeneratorMonad m where
    nextPuzzle :: m Solution
