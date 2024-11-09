{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}

module Hangman.Application.CreateGame
    ( createGame
    , createRandomGame
    ) where

import           Effectful                 (Eff, (:>))
import           Hangman.Application.Ports (GameEffect, PuzzleGeneratorEffect,
                                            nextPuzzle, saveGame)
import           Hangman.Model.Game        (Chances, GameId, createNewGame)
import           Hangman.Model.Puzzle      (Solution)
import           Hangman.Named             (name)

createGame :: (GameEffect :> es) => GameId -> Chances -> Solution -> Eff es ()
createGame gameId chances solution =
    name gameId $ \namedGameId ->
        saveGame namedGameId $ createNewGame solution chances

createRandomGame
    :: PuzzleGeneratorEffect :> es
    => GameEffect :> es
    => GameId
    -> Chances
    -> Eff es ()
createRandomGame gameId chances = do
    solution <- nextPuzzle
    createGame gameId chances solution
