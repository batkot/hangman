{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Hangman.Application.GuessLetter
    ( guessLetter
    , GuessLetterError(..)) where

import           Effectful                 (Eff, (:>))
import           Effectful.Error.Dynamic   (Error, throwError)
import           Hangman.Application.Ports (GameEffect, findGame, saveGame)
import qualified Hangman.Model.Game        as Game
import           Hangman.Named             (name)

newtype GuessLetterError = GameNotFound Game.GameId

guessLetter
    :: GameEffect :> es
    => Error GuessLetterError :> es
    => Game.GameId
    -> Char
    -> Eff es ()
guessLetter gameId guess =
    name gameId $ \namedGameId -> do
        game <- findGame namedGameId >>= maybe (throwError (GameNotFound gameId)) return
        let saveGame' = saveGame namedGameId
            doSetGame = either (either saveGame' saveGame') saveGame'

        doSetGame $ Game.guessLetter guess game
