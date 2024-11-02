{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Hangman.Server.GamesSpec
    ( tests
    ) where

import           Control.Monad                   (foldM_, void)
import           Data.List                       (nub)
import           Data.List.Extra                 (nubOrdOn)
import           Data.Text                       (pack, unpack)
import           GHC.Unicode                     (toUpper)
import           Hangman.Model.Game              (GameState (..))
import           Hangman.Server.API.Games        (CreateGameRequest (..),
                                                  GameDescriptionResponse (..))
import           Hangman.Server.Resources.WebApp (GamesClient (..),
                                                  WebClient (..))
import           Network.HTTP.Types.Status       (status404)
import           Servant.Client                  (ClientError (FailureResponse),
                                                  ResponseF (Response),
                                                  runClientM)
import           Test.QuickCheck                 (Arbitrary (..), InfiniteList,
                                                  getNonEmpty, ioProperty)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                ((@?=))
import           Test.Tasty.QuickCheck           (InfiniteList (..), Property,
                                                  testProperty)

newtype ValidRequest a = ValidRequest a deriving newtype (Show, Eq)
newtype InvalidRequest a = InvalidRequest a deriving newtype (Show, Eq)

instance Arbitrary (ValidRequest CreateGameRequest) where
    arbitrary = ValidRequest . CreateGameRequest . pack . getNonEmpty <$> arbitrary

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "Hangman Game tests"
        [ testProperty "Created game should be retrievable by id" $ createdGameShouldBeRetrievableById webClientM
        , testProperty "Guessing Letter call should return current puzzle state" $ guessingLetterShouldReturnCurrentPuzzleState webClientM
        , testProperty "Given Puzzle in CreateGameRequest should use it as puzzle" $ shouldUsePuzzleGivenOnCreation webClientM
        , testProperty "Guessing letter on finished game should return 404" $ guessingLetterOnFinishedGameShouldReturnError webClientM
        , testProperty "Guessing wrong letters should lose game" $ guessingWrongLettersShouldLoseGame webClientM
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
        guesses = nubOrdOn toUpper $ unpack puzzle

    foldM_ (\_ guess -> runClientM (guessLetter games createdGameId guess) env) (Right createGameResp) guesses

    Right gameStatus <- runClientM (getGame games createdGameId) env

    let expectedPuzzle = pack . fmap toUpper . unpack $ puzzle
    gameStatus @?= GameDescriptionResponse createdGameId Won Nothing expectedPuzzle

guessingLetterOnFinishedGameShouldReturnError :: IO WebClient -> ValidRequest CreateGameRequest -> Char -> Property
guessingLetterOnFinishedGameShouldReturnError webClientM (ValidRequest createGameRequest) finalGuess = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
        (CreateGameRequest puzzle) = createGameRequest
        guesses = nubOrdOn toUpper . unpack $ puzzle

    mapM_ (\guess -> runClientM (guessLetter games createdGameId guess) env) guesses

    Left (FailureResponse _ (Response responseStatus _ _ _)) <- runClientM (guessLetter games createdGameId finalGuess) env
    responseStatus @?= status404

guessingWrongLettersShouldLoseGame :: IO WebClient -> ValidRequest CreateGameRequest -> InfiniteList Char -> Property
guessingWrongLettersShouldLoseGame webClientM (ValidRequest createGameRequest) infiniteChars = ioProperty $ do
    WebClient{..} <- webClientM
    Right createGameResp <- runClientM (createGame games createGameRequest) env
    let createdGameId = gameId createGameResp
        (CreateGameRequest gamePuzzle) = createGameRequest
        puzzleChars = nub . fmap toUpper . unpack $ gamePuzzle
        wrongGuesses = filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars
        runGame [] x = return x
        runGame (wrongGuess : gs) curr =
            case curr of
            Right _ -> runClientM (guessLetter games createdGameId wrongGuess) env >>= runGame gs
            x -> return x

    void $ runGame wrongGuesses (Right createGameResp)

    Right gameStatus <- runClientM (getGame games createdGameId) env

    let expectedPuzzle = puzzle createGameResp
    gameStatus @?= GameDescriptionResponse createdGameId Lost Nothing expectedPuzzle
