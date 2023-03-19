{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hangman.Application.Ports
    ( GameMonad(..)
    , PuzzleGeneratorMonad(..)
    ) where

import Hangman.Model.Game (Game, GameState(..))
import Hangman.Model.Puzzle (Solution)

class Monad m => GameMonad m where
    getGame :: m (Game gameId 'Running)
    setGame :: Game gameId state -> m ()

class Monad m => PuzzleGeneratorMonad m where
    nextPuzzle :: m Solution
