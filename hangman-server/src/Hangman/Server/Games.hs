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
    , CreateGameResponse(..)
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
import           Hangman.Model.Game              (Game (RunningGame),
                                                  GameId (..),
                                                  GameState (Running))
import qualified Hangman.Model.PositiveInt       as PositiveInt
import           Hangman.Model.Puzzle            (describePuzzle)
import qualified Hangman.Named                   as Named
import           Servant                         (Capture, Get, JSON, Post,
                                                  ReqBody,
                                                  ServerError (errBody), err400,
                                                  (:<|>) (..), (:>))
import           Servant.Server                  (ServerT)

newtype CreateGameRequest = CreateGameRequest (Maybe Text)
    deriving stock (Generic, Show, Eq)

instance ToJSON CreateGameRequest
instance FromJSON CreateGameRequest

instance ToSchema CreateGameRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Create new Hangman puzzle"
        & mapped.schema.example ?~ toJSON (CreateGameRequest Nothing)

data CreateGameResponse = CreateGameResponse
    { gameId      :: Text
    , puzzleState :: Text
    }
    deriving stock (Generic, Show, Eq)

instance ToJSON CreateGameResponse
instance FromJSON CreateGameResponse

instance ToSchema CreateGameResponse where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Response from creating new game, containing new game Id"
        & mapped.schema.example ?~ toJSON (CreateGameResponse "f9ffdc08-3e36-4fff-bad4-1d742ca3da7e" "_ _ Z Z _ E")

type Api =
    ReqBody '[JSON] CreateGameRequest :> Post '[JSON] CreateGameResponse
    :<|> Capture "gameId" Text :> Get '[JSON] CreateGameResponse
    :<|> Capture "gameId" Text :> "guess" :> Capture "letter" Char :> Post '[JSON] CreateGameResponse

api :: GameMonad m
    => PuzzleGeneratorMonad m
    => MonadError ServerError m
    => MonadIO m
    => ServerT Api m
api = createGameHandler :<|> getGameHandler :<|> guessLetterHandler

describeGame :: Game gameId 'Running -> Text
describeGame (RunningGame unsolvedPuzzle _) = pack . intersperse ' ' . toList $ fromMaybe '_' <$> describePuzzle unsolvedPuzzle

parseGameId :: MonadError ServerError m => Text -> m GameId
parseGameId rawGameId =
    maybe (throwError badRequestError) (return . GameId) $ UUID.fromText rawGameId
  where
    badRequestError =  err400 { errBody = (BSL.fromStrict . Text.encodeUtf8) rawGameId <> " is not valid GameId" }

createGameHandler
    :: GameMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => CreateGameRequest
    -> m CreateGameResponse
createGameHandler (CreateGameRequest puzzle) = do
    let chances = PositiveInt.increment . PositiveInt.increment $ PositiveInt.one
    gameId <- liftIO $ GameId <$> UUID.nextRandom
    maybe (CreateGame.createRandomGame gameId chances) (CreateGame.createGame gameId chances) $ puzzle >>= NonEmpty.nonEmpty . unpack
    gameDescription <- Named.name gameId (fmap describeGame . getGame)
    let rawGameId = UUID.toText . unGameId $ gameId
    return $ CreateGameResponse rawGameId gameDescription

getGameHandler
    :: GameMonad m
    => MonadError ServerError m
    => Text
    -> m CreateGameResponse
getGameHandler rawGameId = do
    gameId <- parseGameId rawGameId
    gameDescription <- Named.name gameId (fmap describeGame . getGame)
    return $ CreateGameResponse rawGameId gameDescription

guessLetterHandler
    :: GameMonad m
    => Text
    -> Char
    -> m CreateGameResponse
guessLetterHandler rawGameId guess = do
    GuessLetter.guessLetter gameId guess
    gameDescription <- Named.name gameId (fmap describeGame . getGame)
    return $ CreateGameResponse rawGameId gameDescription
  where
    gameId = GameId . fromJust . UUID.fromText $ rawGameId
