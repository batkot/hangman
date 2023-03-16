{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hangman.Application.Ports
    ( AnyGame(..)
    , GameMonad(..)
    , PuzzleGeneratorMonad(..)
    ) where

import Hangman.Model.Game (Game, GameState(..))
import Hangman.Model.Puzzle (Solution)

data AnyGame = forall (state :: GameState). AnyGame (Game state)

class Monad m => GameMonad m where
    getGame :: m (Game 'Running)
    setGame :: AnyGame -> m ()

class Monad m => PuzzleGeneratorMonad m where
    nextPuzzle :: m Solution
