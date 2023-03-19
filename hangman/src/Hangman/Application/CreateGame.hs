{-# LANGUAGE DerivingStrategies #-}

module Hangman.Application.CreateGame
    ( createGame
    , createRandomGame
    ) where

import Hangman.Model.Puzzle (Solution)
import Hangman.Application.Ports (GameMonad(..), PuzzleGeneratorMonad (nextPuzzle))
import Hangman.Model.Game (GameId, Chances, createNewGame)
import Hangman.Named (name)

createGame :: GameMonad m => GameId -> Chances -> Solution -> m ()
createGame gameId chances solution =
    name gameId $ \namedGameId ->
        setGame namedGameId $ createNewGame solution chances

createRandomGame
    :: PuzzleGeneratorMonad m
    => GameMonad m
    => GameId
    -> Chances
    -> m ()
createRandomGame gameId chances = do
    solution <- nextPuzzle
    createGame gameId chances solution

