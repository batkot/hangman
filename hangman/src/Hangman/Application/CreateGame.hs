{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Hangman.Application.CreateGame
    ( createGame
    , createRandomGame
    ) where

import Hangman.Model.Puzzle (Solution)
import Hangman.Application.Ports (GameMonad(..), PuzzleGeneratorMonad (nextPuzzle))
import Hangman.Model.Game (GameId, Chances, createNewGame)
import Hangman.Named (name)

data Command = Command
    { gameId :: GameId
    , solution :: Solution
    , chances :: Chances
    } deriving stock (Eq,Show)

createGame :: GameMonad m => Command -> m ()
createGame Command{..} =
    name gameId $ \namedGameId ->
        setGame namedGameId $ createNewGame solution chances

createRandomGame
    :: PuzzleGeneratorMonad m
    => GameMonad m
    => Chances
    -> GameId
    -> m ()
createRandomGame chances gameId = do
    solution <- nextPuzzle
    createGame Command{..}

