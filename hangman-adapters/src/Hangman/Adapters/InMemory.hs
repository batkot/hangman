{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}


module Hangman.Adapters.InMemory
    ( runGameEffectInMem
    , runConstPuzzleGen
    , runGameReadEffectInMem
    ) where

import           Control.Monad              ((<=<))
import           Data.Either.Extra          (eitherToMaybe, maybeToEither)
import           Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.UUID                  (UUID, toString)
import           Effectful
import           Effectful.Dispatch.Dynamic (interpret)
import           Hangman.Application.Ports  (GameEffect (..),
                                             PuzzleGeneratorEffect (..))
import           Hangman.Model.Game         (Game (..), GameId (..),
                                             GameState (..))
import           Hangman.Model.Puzzle       (Solution)
import           Hangman.Named              (unName)
import           Hangman.Read.Game          (GameReadEffect (..), describeGame)

data AnyGame = forall gameId (state :: GameState). AnyGame (Game gameId state)

type GameStorage = IORef (HM.HashMap UUID AnyGame)

runGameEffectInMem
  :: IOE :> es
  => GameStorage
  -> Eff (GameEffect : es) a
  -> Eff es a
runGameEffectInMem gameStore = interpret $ \_ -> \case
  FindGame gameId -> do
    gameMap <- liftIO $ readIORef gameStore
    let rawGameId :: UUID
        rawGameId = unGameId . unName $ gameId

        matchGame :: AnyGame -> Either String (Game gameId 'Running)
        matchGame (AnyGame (RunningGame a b)) = Right $ RunningGame a b
        matchGame _ = Left $ toString rawGameId <> "is not a running game"

        game = matchGame <=< maybeToEither ("Couldn't find game: " <> toString rawGameId) $ HM.lookup rawGameId gameMap
    return $ eitherToMaybe game

  SaveGame gameId game -> do
    liftIO $ atomicModifyIORef' gameStore $ (,()) . HM.insert ((unGameId . unName) gameId) (AnyGame game)

runGameReadEffectInMem
  :: IOE :> es
  => GameStorage
  -> Eff (GameReadEffect : es) a
  -> Eff es a
runGameReadEffectInMem gameStore = interpret $ \_ -> \case
  FindGameDescription gameId -> do
      let rawGameId :: UUID
          rawGameId = unGameId . unName $ gameId

          doDescribeGame (AnyGame (RunningGame x y)) = describeGame gameId $ RunningGame x y
          doDescribeGame (AnyGame (LostGame x)) = describeGame gameId $ LostGame x
          doDescribeGame (AnyGame (WonGame x)) = describeGame gameId $ WonGame x
      gameMap <- liftIO $ readIORef gameStore
      return $ doDescribeGame <$> HM.lookup rawGameId gameMap

runConstPuzzleGen :: Solution -> Eff (PuzzleGeneratorEffect : es) a -> Eff es a
runConstPuzzleGen solution = interpret $ \_ -> \case
  NextPuzzle -> pure solution
