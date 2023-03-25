{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Hangman.Server.GamesSpec
    ( tests
    ) where

import           Data.List.Extra                 (nubOrdOn)
import           Data.Text                       (intersperse, pack, unpack)
import qualified Data.Text                       as Text
import           GHC.Unicode                     (toUpper)
import           Hangman.Model.Game              (GameState (..))
import           Hangman.Server.Games            (CreateGameRequest (..),
                                                  GameDescriptionResponse (..))
import           Hangman.Server.Resources.WebApp (GamesClient (..),
                                                  WebClient (..))
import           Servant.Client                  (runClientM)
import           Test.QuickCheck                 (Arbitrary (..), getNonEmpty,
                                                  ioProperty)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                ((@?=))
import           Test.Tasty.QuickCheck           (Property, testProperty)

newtype ValidRequest a = ValidRequest a deriving newtype (Show, Eq)
newtype InvalidRequest a = InvalidRequest a deriving newtype (Show, Eq)

instance Arbitrary (ValidRequest CreateGameRequest) where
    arbitrary = ValidRequest . CreateGameRequest . Just . pack . getNonEmpty <$> arbitrary

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "Hangman Game tests"
        [ testProperty "Created game should be retrievable by id" $ createdGameShouldBeRetrievableById webClientM
        , testProperty "Guessing Letter call should return current puzzle state" $ guessingLetterShouldReturnCurrentPuzzleState webClientM
        , testProperty "Given Puzzle in CreateGameRequest should use it as puzzle" $ shouldUsePuzzleGivenOnCreation webClientM
        ]

createdGameShouldBeRetrievableById :: IO WebClient -> ValidRequest CreateGameRequest -> Property
createdGameShouldBeRetrievableById webClientM (ValidRequest createGameRequest) = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
    Right getGameResp <- runClientM (getGame games createdGameId) env
    createGameResp @?= getGameResp

guessingLetterShouldReturnCurrentPuzzleState :: IO WebClient -> ValidRequest CreateGameRequest -> Char -> Property
guessingLetterShouldReturnCurrentPuzzleState webClientM (ValidRequest createGameRequest) guess = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
    Right guessLetterResp <- runClientM (guessLetter games createdGameId guess) env
    Right getGameResp <- runClientM (getGame games createdGameId) env
    guessLetterResp @?= getGameResp

shouldUsePuzzleGivenOnCreation :: IO WebClient -> ValidRequest CreateGameRequest -> Property
shouldUsePuzzleGivenOnCreation webClientM (ValidRequest createGameRequest) = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
        (CreateGameRequest puzzle) = createGameRequest
        guesses = nubOrdOn toUpper $ maybe [] unpack puzzle

    results <- mapM (\guess -> runClientM (guessLetter games createdGameId guess) env) guesses

    last results @?= Right (GameDescriptionResponse createdGameId Won Nothing (maybe Text.empty Text.toUpper puzzle))

-- Guessing letters on finished game should return 404 (or 409?)
