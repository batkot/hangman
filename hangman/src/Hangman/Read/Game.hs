{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Hangman.Read.Game
    ( GameDescription(..)
    , GameReadMonad(..)
    , describeGame
    ) where

import           Hangman.Model.Game        (Game (..), GameId, GameState (..))
import           Hangman.Model.PositiveInt (PositiveInt)
import           Hangman.Named             (Named)

import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Foldable             (toList)
import           Data.List                 (intersperse)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, pack)
import           Hangman.Model.Puzzle      (describePuzzle, getSolution)

data GameDescription gameId = GameDescription
    { id          :: Named gameId GameId
    , state       :: GameState
    , chancesLeft :: Maybe PositiveInt
    , puzzle      :: Text
    }

class Monad m => GameReadMonad m where
    findGameDescription :: Named gameId GameId -> m (Maybe (GameDescription gameId))

instance {-# OVERLAPPABLE #-}
    ( GameReadMonad m
    , MonadTrans t
    , Monad (t m)) => GameReadMonad (t m) where
  findGameDescription = lift . findGameDescription

describeGame :: Named gameId GameId -> Game gameId state -> GameDescription gameId
describeGame gameId = \case
    RunningGame puzzle chances -> GameDescription gameId Running (Just chances) $ describeUnsolved puzzle
    LostGame x -> GameDescription gameId Lost Nothing $ describeUnsolved x
    WonGame x -> GameDescription gameId Won Nothing $ pack . toList . getSolution $ x
  where
    describeUnsolved = pack . intersperse ' ' . toList . fmap (fromMaybe '_') . describePuzzle
