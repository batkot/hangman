{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE UndecidableInstances      #-}

module Hangman.Application.Ports
    ( GameMonad(..)
    , PuzzleGeneratorMonad(..)
    ) where

import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Hangman.Model.Game        (Game, GameId, GameState (..))
import           Hangman.Model.Puzzle      (Solution)
import           Hangman.Named             (Named)

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
