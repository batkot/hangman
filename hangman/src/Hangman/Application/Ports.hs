{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hangman.Application.Ports
    ( GameMonad(..)
    , PuzzleGeneratorMonad(..)
    ) where

import Hangman.Model.Game (Game, GameState(..), GameId)
import Hangman.Model.Puzzle (Solution)
import Hangman.Named (Named)
import Control.Monad.Trans.Class (MonadTrans, lift)

class Monad m => GameMonad m where
    getGame :: Named gameId GameId -> m (Game gameId 'Running)
    setGame :: Named gameId GameId -> Game gameId state -> m ()

instance {-# OVERLAPPABLE #-}
    ( GameMonad m
    , MonadTrans t
    , Monad (t m)) => GameMonad (t m) where
    getGame = lift . getGame
    setGame gameId = lift . setGame gameId

class Monad m => PuzzleGeneratorMonad m where
    nextPuzzle :: m Solution

instance {-# OVERLAPPABLE #-}
    ( PuzzleGeneratorMonad m
    , MonadTrans t
    , Monad (t m)) => PuzzleGeneratorMonad (t m) where
    nextPuzzle = lift nextPuzzle
