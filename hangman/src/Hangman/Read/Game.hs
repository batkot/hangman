{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Hangman.Read.Game
    ( GameDescription(..)
    , describeGame
    , findGameDescription
    , GameReadEffect(..)
    ) where

import           Hangman.Model.Game         (Game (..), GameId, GameState (..))
import           Hangman.Model.PositiveInt  (PositiveInt)
import           Hangman.Named              (Named)

import           Data.Foldable              (toList)
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, pack)
import           Effectful                  (Dispatch (Dynamic), DispatchOf,
                                             Eff, Effect, (:>))
import           Effectful.Dispatch.Dynamic (send)
import           Hangman.Model.Puzzle       (describePuzzle, getSolution)

data GameDescription gameId = GameDescription
    { id          :: Named gameId GameId
    , state       :: GameState
    , chancesLeft :: Maybe PositiveInt
    , puzzle      :: Text
    }

data GameReadEffect :: Effect where
  FindGameDescription :: Named gameId GameId -> GameReadEffect m (Maybe (GameDescription gameId))

type instance DispatchOf GameReadEffect = Dynamic

findGameDescription :: GameReadEffect :> es => Named gameId GameId -> Eff es (Maybe (GameDescription gameId))
findGameDescription = send . FindGameDescription

describeGame :: Named gameId GameId -> Game gameId state -> GameDescription gameId
describeGame gameId = \case
    RunningGame puzzle chances -> GameDescription gameId Running (Just chances) $ describeUnsolved puzzle
    LostGame x -> GameDescription gameId Lost Nothing $ describeUnsolved x
    WonGame x -> GameDescription gameId Won Nothing $ pack . toList . getSolution $ x
  where
    describeUnsolved = pack . intersperse ' ' . toList . fmap (fromMaybe '_') . describePuzzle
