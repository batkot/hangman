module Hangman.Application.GuessLetter
    ( guessLetter
    ) where

import           Hangman.Application.Ports (GameMonad (..))
import qualified Hangman.Model.Game        as Game
import           Hangman.Named             (name)

guessLetter :: GameMonad m => Game.GameId -> Char -> m ()
guessLetter gameId guess =
    name gameId $ \namedGameId -> do
        game <- getGame namedGameId
        let setGame' = setGame namedGameId
            doSetGame = either (either setGame' setGame') setGame'
        doSetGame $ Game.guessLetter guess game

