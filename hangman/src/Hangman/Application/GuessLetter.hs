{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.GuessLetter
    ( guessLetter
    ) where

import qualified Hangman.Model.Game as Game
import Hangman.Application.Ports (AnyGame(..), GameMonad(..))

newtype Command = Command { guess :: Char } deriving newtype (Eq,Show)

guessLetter :: GameMonad m => Command -> m ()
guessLetter Command{..} = do
    game <- getGame
    setGame . wrapGame $ Game.guessLetter guess game
  where
    wrapGame = either (either AnyGame AnyGame) AnyGame

