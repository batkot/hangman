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

import           Control.Monad              ((<=<))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.Bifunctor             (bimap)
import           Data.UUID                  (UUID, toString)
import           Hangman.Application.Ports  (GameMonad (..),
                                             PuzzleGeneratorMonad (..))
import           Hangman.Model.Game         (Game (RunningGame), GameId (..),
                                             GameState (..))
import           Hangman.Model.Puzzle       (Solution)
import           Hangman.Named              (Named, unName)
import Data.Either.Extra (maybeToEither, fromRight, fromEither)

data AnyGame = forall gameId (state :: GameState). AnyGame (Game gameId state)

type GameStorage = IORef (HM.HashMap UUID AnyGame)

newtype InMemoryGameStorageT m a =
    InMemoryGameStorageT { unInMemoryGameStorageT :: ReaderT GameStorage m a }
        deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

runInMemoryGameStorageT
    :: IORef (HM.HashMap UUID AnyGame)
    -> InMemoryGameStorageT m a
    -> m a
runInMemoryGameStorageT gameMapIoRef = flip runReaderT gameMapIoRef . unInMemoryGameStorageT

instance MonadIO m => GameMonad (InMemoryGameStorageT m) where
  getGame :: Named gameId GameId -> InMemoryGameStorageT m (Game gameId 'Running)
  getGame gameId = InMemoryGameStorageT $ do
      ioRef <- ask
      gameMap <- liftIO $ readIORef ioRef
      let game = findGame <=< maybeToEither ("Couldn't find game: " <> toString rawGameId) $ HM.lookup rawGameId gameMap
      liftIO $ fromEither $ bimap fail return game
    where
      rawGameId :: UUID
      rawGameId = unGameId . unName $ gameId

      findGame :: AnyGame -> Either String (Game gameId 'Running)
      findGame (AnyGame (RunningGame a b)) = Right $ RunningGame a b
      findGame _ = Left $ toString rawGameId <> "is not a running game"

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
