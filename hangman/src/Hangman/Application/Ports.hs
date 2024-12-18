{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Hangman.Application.Ports
    ( GameEffect(..)
    , findGame
    , saveGame
    , nextId
    , PuzzleGeneratorEffect(..)
    , nextPuzzle
    ) where

import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, (:>))
import           Effectful.Dispatch.Dynamic (send)
import           Hangman.Model.Game         (Game, GameId, GameState (..))
import           Hangman.Model.Puzzle       (Solution)
import           Hangman.Named              (Named)

data GameEffect :: Effect where
    -- TODO: Separate effects
    NextId   :: GameEffect m GameId
    FindGame :: Named gameId GameId -> GameEffect m (Maybe (Game gameId 'Running))
    SaveGame :: Named gameId GameId -> Game gameId state -> GameEffect m ()

type instance DispatchOf GameEffect = Dynamic

nextId :: (GameEffect :> es) => Eff es GameId
nextId = send NextId

findGame :: (GameEffect :> es) => Named gameId GameId -> Eff es (Maybe (Game gameId 'Running))
findGame = send . FindGame

saveGame :: (GameEffect :> es) => Named gameId GameId -> Game gameId state -> Eff es ()
saveGame gameId = send . SaveGame gameId

data PuzzleGeneratorEffect :: Effect where
    NextPuzzle :: PuzzleGeneratorEffect m Solution

type instance DispatchOf PuzzleGeneratorEffect = Dynamic

nextPuzzle :: (PuzzleGeneratorEffect :> es) => Eff es Solution
nextPuzzle = send NextPuzzle
