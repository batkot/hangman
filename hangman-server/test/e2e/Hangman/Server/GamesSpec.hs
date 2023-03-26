{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Hangman.Server.GamesSpec
    ( tests
    ) where

import           Control.Monad                   (foldM_)
import           Data.List.Extra                 (nubOrdOn)
import           Data.Text                       (pack, unpack)
import qualified Data.Text                       as Text
import           GHC.Unicode                     (toUpper)
import           Hangman.Model.Game              (GameState (..))
import           Hangman.Server.Games            (CreateGameRequest (..),
                                                  GameDescriptionResponse (..))
import           Hangman.Server.Resources.WebApp (GamesClient (..),
                                                  WebClient (..))
import           Network.HTTP.Types.Status       (status404)
import           Servant.Client                  (ClientError (FailureResponse),
                                                  ResponseF (Response),
                                                  runClientM)
import           Test.QuickCheck                 (ASCIIString (getASCIIString),
                                                  Arbitrary (..), getNonEmpty,
                                                  ioProperty, listOf, listOf1)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                ((@?=))
import           Test.Tasty.QuickCheck           (Property, elements, suchThat,
                                                  testProperty)

newtype ValidRequest a = ValidRequest a deriving newtype (Show, Eq)
newtype InvalidRequest a = InvalidRequest a deriving newtype (Show, Eq)

instance Arbitrary (ValidRequest CreateGameRequest) where
    arbitrary = do
        asciiString <- (`suchThat` ((<) 0 . length)) $ getASCIIString <$> arbitrary
        return . ValidRequest . CreateGameRequest . Just . pack $ asciiString

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "Hangman Game tests"
        [ testProperty "Created game should be retrievable by id" $ createdGameShouldBeRetrievableById webClientM
        , testProperty "Guessing Letter call should return current puzzle state" $ guessingLetterShouldReturnCurrentPuzzleState webClientM
        , testProperty "Given Puzzle in CreateGameRequest should use it as puzzle" $ shouldUsePuzzleGivenOnCreation webClientM
        , testProperty "Guessing letter on finished game should return 404" $ guessingLetterOnFinishedGameShouldReturnError webClientM
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

    foldM_ (\_ guess -> runClientM (guessLetter games createdGameId guess) env) (Right createGameResp) guesses

    Right gameStatus <- runClientM (getGame games createdGameId) env

    gameStatus @?= GameDescriptionResponse createdGameId Won Nothing (maybe Text.empty Text.toUpper puzzle)

guessingLetterOnFinishedGameShouldReturnError :: IO WebClient -> ValidRequest CreateGameRequest -> Char -> Property
guessingLetterOnFinishedGameShouldReturnError webClientM (ValidRequest createGameRequest) finalGuess = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
        (CreateGameRequest puzzle) = createGameRequest
        guesses = nubOrdOn toUpper $ maybe [] unpack puzzle

    foldM_ (\_ guess -> runClientM (guessLetter games createdGameId guess) env) (Right createGameResp) guesses

    Left (FailureResponse _ (Response responseStatus _ _ _)) <- runClientM (guessLetter games createdGameId finalGuess) env
    responseStatus @?= status404

