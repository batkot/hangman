{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.GuessLetter
    ( guessLetter
    ) where

import qualified Hangman.Model.Game as Game
import Hangman.Application.Ports (GameMonad(..))
import Hangman.Named (name)

data Command = Command
    { gameId :: Game.GameId
    , guess :: Char }
    deriving stock (Eq,Show)

guessLetter :: GameMonad m => Command -> m ()
guessLetter Command{..} =
    name gameId $ \namedGameId -> do
        game <- getGame namedGameId
        let setGame' = setGame namedGameId
            doSetGame = either (either setGame' setGame') setGame'
        doSetGame $ Game.guessLetter guess game

