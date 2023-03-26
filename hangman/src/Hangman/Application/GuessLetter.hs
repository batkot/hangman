module Hangman.Application.GuessLetter
    ( guessLetter
    , GuessLetterError(..)) where

import           Hangman.Application.Ports (GameMonad (..))
import qualified Hangman.Model.Game        as Game
import           Hangman.Named             (name)
import Control.Monad.Trans.Except (ExceptT, throwE)

newtype GuessLetterError = GameNotFound Game.GameId

guessLetter
    :: GameMonad m => Game.GameId -> Char -> ExceptT GuessLetterError m ()
guessLetter gameId guess =
    name gameId $ \namedGameId -> do
        game <- findGame namedGameId >>= maybe (throwE (GameNotFound gameId)) return
        let saveGame' = saveGame namedGameId
            doSetGame = either (either saveGame' saveGame') saveGame'

        doSetGame $ Game.guessLetter guess game
