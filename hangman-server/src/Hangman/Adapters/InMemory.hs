{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE InstanceSigs               #-}

module Hangman.Adapters.InMemory
    ( runInMemoryGameStorageT
    , runConstPuzzleGenT
    ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.Maybe                 (fromJust)
import           Data.UUID                  (UUID)
import           Hangman.Application.Ports  (GameMonad (..),
                                             PuzzleGeneratorMonad (..))
import           Hangman.Model.Game         (Game (RunningGame), GameId (..),
                                             GameState (..))
import           Hangman.Model.Puzzle       (Solution)
import           Hangman.Named              (Named, unName)

data AnyGame = forall gameId (state :: GameState). AnyGame (Game gameId state)

type GameStorage = IORef (HM.HashMap UUID AnyGame)

newtype InMemoryGameStorageT m a =
    InMemoryGameStorageT { unInMemoryGameStorageT :: ReaderT GameStorage m a }
        deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

runInMemoryGameStorageT
    :: MonadIO m
    => HM.HashMap UUID AnyGame
    -> InMemoryGameStorageT m a
    -> m a
runInMemoryGameStorageT gameMap inMemT = do
    ioRef <- liftIO $ newIORef gameMap
    flip runReaderT ioRef $ unInMemoryGameStorageT inMemT

instance MonadIO m => GameMonad (InMemoryGameStorageT m) where
  getGame :: Named gameId GameId -> InMemoryGameStorageT m (Game gameId 'Running)
  getGame gameId = InMemoryGameStorageT $ do
      ioRef <- ask
      gameMap <- liftIO $ readIORef ioRef
      let (GameId x) = unName gameId
      return $ fromJust undefined $ HM.lookup x gameMap >>= findGame
    where
      findGame :: AnyGame -> Maybe (Game gameId 'Running)
      findGame (AnyGame (RunningGame a b)) = Just $ RunningGame a b
      findGame _                           = Nothing

  setGame :: Named gameId GameId -> Game gameId state -> InMemoryGameStorageT m ()
  setGame gameId game = InMemoryGameStorageT $ do
      ioRef <- ask
      liftIO $ modifyIORef ioRef $ HM.insert ((unGameId . unName) gameId) (AnyGame game)

newtype ConstPuzzleGenT m a =
    ConstPuzzleGenT { unConstPuzzleGenT :: ReaderT Solution m a }
        deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

runConstPuzzleGenT :: Solution -> ConstPuzzleGenT m a -> m a
runConstPuzzleGenT solution = flip runReaderT solution . unConstPuzzleGenT

instance Monad m => PuzzleGeneratorMonad (ConstPuzzleGenT m) where
  nextPuzzle = ConstPuzzleGenT ask
