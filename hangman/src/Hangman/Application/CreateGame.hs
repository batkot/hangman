{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}

module Hangman.Application.CreateGame
    ( createGame
    , createRandomGame
    ) where

import           Effectful                 (Eff, (:>))
import           Hangman.Application.Ports (GameEffect, PuzzleGeneratorEffect,
                                            nextId, nextPuzzle, saveGame)
import           Hangman.Model.Game        (Chances, GameId, createNewGame)
import           Hangman.Model.Puzzle      (Solution)
import           Hangman.Named             (name)

createGame :: (GameEffect :> es) => Chances -> Solution -> Eff es GameId
createGame chances solution = do
    gameId <- nextId
    name gameId $ \namedGameId -> do
        saveGame namedGameId $ createNewGame solution chances
        pure gameId

createRandomGame
    :: PuzzleGeneratorEffect :> es
    => GameEffect :> es
    => Chances
    -> Eff es GameId
createRandomGame chances = nextPuzzle >>= createGame chances
