{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.GuessLetter
    ( guessLetter
    ) where

import qualified Hangman.Model.Game as Game
import Hangman.Application.Ports (GameMonad(..))

newtype Command = Command { guess :: Char } deriving newtype (Eq,Show)

guessLetter :: GameMonad m => Command -> m ()
guessLetter Command{..} = do
    game <- getGame
    doSetGame $ Game.guessLetter guess game
  where
    doSetGame = either (either setGame setGame) setGame

