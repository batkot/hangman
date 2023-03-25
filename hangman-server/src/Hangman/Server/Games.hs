{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Hangman.Server.Games
    ( Api
    , api
    , CreateGameRequest(..)
    , GameDescriptionResponse(..)
    ) where

import           Control.Lens
import           Control.Monad.Error.Class       (MonadError, throwError)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Aeson                      (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString.Lazy            as BSL
import           Data.List                       (intersperse)
import           Data.List.NonEmpty              (toList)
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromJust, fromMaybe)
import           Data.Swagger
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text.Encoding              as Text
import           Data.UUID                       as UUID
import           Data.UUID.V4                    as UUID
import           GHC.Generics                    (Generic)
import           Hangman.Application.CreateGame  as CreateGame
import           Hangman.Application.GuessLetter as GuessLetter
import           Hangman.Application.Ports       (GameMonad (getGame),
                                                  PuzzleGeneratorMonad)
import           Hangman.Model.Game              (Game (LostGame, RunningGame, WonGame),
                                                  GameId (..), GameState (..))
import qualified Hangman.Model.PositiveInt       as PositiveInt
import           Hangman.Model.Puzzle            (describePuzzle, getSolution)
import           Hangman.Named                   (Named, unName)
import qualified Hangman.Named                   as Named
import           Hangman.Read.Game               (GameDescription (..),
                                                  GameReadMonad (..))
import           Servant                         (Capture, Get, JSON, Post,
                                                  ReqBody,
                                                  ServerError (errBody), err400,
                                                  err404, (:<|>) (..), (:>))
import           Servant.Server                  (ServerT)

newtype CreateGameRequest = CreateGameRequest (Maybe Text)
    deriving stock (Generic, Show, Eq)

instance ToJSON CreateGameRequest
instance FromJSON CreateGameRequest

instance ToSchema CreateGameRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Create new Hangman puzzle"
        & mapped.schema.example ?~ toJSON (CreateGameRequest Nothing)

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
    gameId <- liftIO $ GameId <$> UUID.nextRandom
    maybe (CreateGame.createRandomGame gameId chances) (CreateGame.createGame gameId chances) $ puzzle >>= NonEmpty.nonEmpty . unpack
    maybeGameDescription <- getGameDescription gameId
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
    => Text
    -> Char
    -> m GameDescriptionResponse
guessLetterHandler rawGameId guess = do
    GuessLetter.guessLetter gameId guess
    maybeGameDescription <- getGameDescription gameId
    return $ fromJust maybeGameDescription
  where
    gameId = GameId . fromJust . UUID.fromText $ rawGameId
