{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Hangman.Server.Game
    ( Api
    , api) where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Aeson                     (FromJSON, ToJSON, toJSON)
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Swagger
import           Data.Text                      (Text, unpack)
import           Data.UUID                      as UUID
import           Data.UUID.V4                   as UUID
import           GHC.Generics                   (Generic)
import           Hangman.Application.CreateGame as CreateGame
import           Hangman.Application.Ports      (GameMonad,
                                                 PuzzleGeneratorMonad)
import           Hangman.Model.Game             (GameId (..))
import qualified Hangman.Model.PositiveInt      as PositiveInt
import           Servant                        (JSON, Post, ReqBody, (:>))
import           Servant.Server                 (ServerT)

newtype CreateGameRequest = CreateGameRequest (Maybe Text)
    deriving stock (Generic)

instance ToJSON CreateGameRequest
instance FromJSON CreateGameRequest

instance ToSchema CreateGameRequest where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Create new Hangman puzzle"
        & mapped.schema.example ?~ toJSON (CreateGameRequest (Just "PUZZLE"))

newtype CreateGameResponse = CreateGameResponse Text
    deriving stock (Generic)

instance ToJSON CreateGameResponse
instance FromJSON CreateGameResponse

instance ToSchema CreateGameResponse where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Response from creating new game, containing new game Id"
        & mapped.schema.example ?~ toJSON (CreateGameResponse "f9ffdc08-3e36-4fff-bad4-1d742ca3da7e")

type Api =
    ReqBody '[JSON] CreateGameRequest :> Post '[JSON] CreateGameResponse

api :: GameMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => ServerT Api m
api = createGameHandler

createGameHandler
    :: GameMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => CreateGameRequest
    -> m CreateGameResponse
createGameHandler (CreateGameRequest puzzle) = do
    let chances = PositiveInt.one
    gameId <- liftIO $ GameId <$> UUID.nextRandom
    maybe (CreateGame.createRandomGame gameId chances) (CreateGame.createGame gameId chances) $ puzzle >>= NonEmpty.nonEmpty . unpack
    return . CreateGameResponse . UUID.toText . unGameId $ gameId
