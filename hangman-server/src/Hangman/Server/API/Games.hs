{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module Hangman.Server.API.Games
    ( Api
    , api
    , CreateGameRequest(..)
    , GameDescriptionResponse(..)
    ) where

import           Control.Lens
import           Control.Monad.IO.Class          (liftIO)
import           Data.Aeson                      (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromJust)
import           Data.Swagger
import           Data.Text                       (Text, unpack)
import qualified Data.Text.Encoding              as Text
import           Data.UUID                       as UUID
import           Data.UUID.V4                    as UUID
import           Effectful                       (Eff, IOE, (:>))
import           Effectful.Error.Dynamic         (Error, runErrorWith,
                                                  throwError)
import           GHC.Generics                    (Generic)
import qualified Hangman.Application.CreateGame  as CreateGame
import           Hangman.Application.GuessLetter (GuessLetterError (..))
import qualified Hangman.Application.GuessLetter as GuessLetter
import           Hangman.Application.Ports       (GameEffect,
                                                  PuzzleGeneratorEffect)
import           Hangman.Model.Game              (GameId (..), GameState (..))
import qualified Hangman.Model.PositiveInt       as PositiveInt
import           Hangman.Named                   (unName)
import qualified Hangman.Named                   as Named
import           Hangman.Read.Game               (GameDescription (..),
                                                  GameReadEffect,
                                                  findGameDescription)
import qualified Servant                         as S
import           Servant                         (Capture, Get, JSON, Post,
                                                  ReqBody,
                                                  ServerError (errBody), err400,
                                                  err404, (:<|>) (..))
import           Servant.Server                  (ServerT)

newtype CreateGameRequest = CreateGameRequest Text
    deriving stock (Generic, Show, Eq)

instance ToJSON CreateGameRequest
instance FromJSON CreateGameRequest

instance ToSchema CreateGameRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Create new Hangman puzzle. If empty string is given in body, game with random puzzle will be created"
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
    ReqBody '[JSON] CreateGameRequest S.:> Post '[JSON] GameDescriptionResponse
    :<|> Capture "gameId" Text S.:> Get '[JSON] GameDescriptionResponse
    :<|> Capture "gameId" Text S.:> "guess" S.:> Capture "letter" Char S.:> Post '[JSON] GameDescriptionResponse

api :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => Error ServerError :> es
    => ServerT Api (Eff es)
api = createGameHandler :<|> getGameHandler :<|> guessLetterHandler

descriptionToDto :: GameDescription gameId -> GameDescriptionResponse
descriptionToDto (GameDescription gameId gameState chances puzzle) =
    GameDescriptionResponse rawGameId gameState rawChances puzzle
  where
    rawGameId = UUID.toText . unGameId . unName $ gameId
    rawChances = PositiveInt.toInt <$> chances

getGameDescription
    :: GameReadEffect :> es
    => GameId
    -> Eff es (Maybe GameDescriptionResponse)
getGameDescription gameId =
    Named.name gameId $ fmap (fmap descriptionToDto) . findGameDescription

parseGameId :: Error ServerError :> es => Text -> Eff es GameId
parseGameId rawGameId =
    maybe (throwError badRequestError) (return . GameId) $ UUID.fromText rawGameId
  where
    badRequestError =  err400 { errBody = (BSL.fromStrict . Text.encodeUtf8) rawGameId <> " is not valid GameId" }

createGameHandler
    :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => CreateGameRequest
    -> Eff es GameDescriptionResponse
createGameHandler (CreateGameRequest puzzle) = do
    let chances = foldr ($) PositiveInt.one $ replicate 9 PositiveInt.increment
    let createRandomGame = CreateGame.createRandomGame chances
        createNewGame = CreateGame.createGame chances
    newGameId <- maybe createRandomGame createNewGame $ NonEmpty.nonEmpty . unpack $ puzzle
    maybeGameDescription <- getGameDescription newGameId
    return $ fromJust maybeGameDescription

getGameHandler
    :: GameReadEffect :> es
    => Error ServerError :> es
    => Text
    -> Eff es GameDescriptionResponse
getGameHandler rawGameId = do
    gameId <- parseGameId rawGameId
    maybeGameDescription <- getGameDescription gameId
    maybe (throwError err404) return maybeGameDescription

guessLetterHandler
    :: GameEffect :> es
    => GameReadEffect :> es
    => Error ServerError :> es
    => Text
    -> Char
    -> Eff es GameDescriptionResponse
guessLetterHandler rawGameId guess = do
    gameId <- parseGameId rawGameId
    runErrorWith (\_ (GameNotFound _) -> throwError err404) $ GuessLetter.guessLetter gameId guess
    maybeGameDescription <- getGameDescription gameId
    return $ fromJust maybeGameDescription
