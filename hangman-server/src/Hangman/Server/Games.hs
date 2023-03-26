{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Hangman.Server.Games
    ( Api
    , api
    , CreateGameRequest(..)
    , GameDescriptionResponse(..)
    ) where

import           Control.Lens
import           Control.Monad.Error.Class       (MonadError, liftEither,
                                                  throwError)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except      (runExceptT)
import           Data.Aeson                      (FromJSON, ToJSON, toJSON)
import           Data.Bifunctor                  (first)
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromJust)
import           Data.Swagger
import           Data.Text                       (Text, unpack)
import qualified Data.Text.Encoding              as Text
import           Data.UUID                       as UUID
import           Data.UUID.V4                    as UUID
import           GHC.Generics                    (Generic)
import qualified Hangman.Application.CreateGame  as CreateGame
import qualified Hangman.Application.GuessLetter as GuessLetter
import           Hangman.Application.Ports       (GameMonad,
                                                  PuzzleGeneratorMonad)
import           Hangman.Model.Game              (GameId (..), GameState (..))
import qualified Hangman.Model.PositiveInt       as PositiveInt
import           Hangman.Named                   (unName)
import qualified Hangman.Named                   as Named
import           Hangman.Read.Game               (GameDescription (..),
                                                  GameReadMonad (..))
import           Servant                         (Capture, Get, JSON, Post,
                                                  ReqBody,
                                                  ServerError (errBody), err400,
                                                  err404, (:<|>) (..), (:>))
import           Servant.Server                  (ServerT)

newtype CreateGameRequest = CreateGameRequest Text
    deriving stock (Generic, Show, Eq)

instance ToJSON CreateGameRequest
instance FromJSON CreateGameRequest

instance ToSchema CreateGameRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Create new Hangman puzzle"
        & mapped.schema.example ?~ toJSON (CreateGameRequest "PUZZLE")

data GameDescriptionResponse = GameDescriptionResponse
    { gameId           :: Text
    , status           :: GameState
    , remainingChances :: Maybe Int
    , puzzle           :: Text
    }
    deriving stock (Generic, Show, Eq)

instance ToJSON GameState
instance FromJSON GameState

instance ToSchema GameState where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToJSON GameDescriptionResponse
instance FromJSON GameDescriptionResponse

instance ToSchema GameDescriptionResponse where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Description of Hangman game"
        & mapped.schema.example ?~ toJSON (GameDescriptionResponse "f9ffdc08-3e36-4fff-bad4-1d742ca3da7e" Running (Just 3) "_ _ Z Z _ E")

type Api =
    ReqBody '[JSON] CreateGameRequest :> Post '[JSON] GameDescriptionResponse
    :<|> Capture "gameId" Text :> Get '[JSON] GameDescriptionResponse
    :<|> Capture "gameId" Text :> "guess" :> Capture "letter" Char :> Post '[JSON] GameDescriptionResponse

api :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadError ServerError m
    => MonadIO m
    => ServerT Api m
api = createGameHandler :<|> getGameHandler :<|> guessLetterHandler

descriptionToDto :: GameDescription gameId -> GameDescriptionResponse
descriptionToDto (GameDescription gameId gameState chances puzzle) =
    GameDescriptionResponse rawGameId gameState rawChances puzzle
  where
    rawGameId = UUID.toText . unGameId . unName $ gameId
    rawChances = PositiveInt.toInt <$> chances

getGameDescription
    :: GameReadMonad m
    => GameId
    -> m (Maybe GameDescriptionResponse)
getGameDescription gameId =
    Named.name gameId $ fmap (fmap descriptionToDto) . findGameDescription

parseGameId :: MonadError ServerError m => Text -> m GameId
parseGameId rawGameId =
    maybe (throwError badRequestError) (return . GameId) $ UUID.fromText rawGameId
  where
    badRequestError =  err400 { errBody = (BSL.fromStrict . Text.encodeUtf8) rawGameId <> " is not valid GameId" }

createGameHandler
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => CreateGameRequest
    -> m GameDescriptionResponse
createGameHandler (CreateGameRequest puzzle) = do
    let chances = foldr ($) PositiveInt.one $ replicate 9 PositiveInt.increment
    newGameId <- liftIO $ GameId <$> UUID.nextRandom
    let createRandomGame = CreateGame.createRandomGame newGameId chances
        createNewGame = CreateGame.createGame newGameId chances
    maybe createRandomGame createNewGame $ NonEmpty.nonEmpty . unpack $ puzzle
    maybeGameDescription <- getGameDescription newGameId
    return $ fromJust maybeGameDescription

getGameHandler
    :: GameMonad m
    => GameReadMonad m
    => MonadError ServerError m
    => Text
    -> m GameDescriptionResponse
getGameHandler rawGameId = do
    gameId <- parseGameId rawGameId
    maybeGameDescription <- getGameDescription gameId
    maybe (throwError err404) return maybeGameDescription

guessLetterHandler
    :: GameMonad m
    => GameReadMonad m
    => MonadError ServerError m
    => Text
    -> Char
    -> m GameDescriptionResponse
guessLetterHandler rawGameId guess = do
    gameId <- parseGameId rawGameId
    result <- runExceptT $ GuessLetter.guessLetter gameId guess
    liftEither $ first (const err404) result
    maybeGameDescription <- getGameDescription gameId
    return $ fromJust maybeGameDescription
