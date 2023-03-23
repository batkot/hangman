{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Hangman.Server
    ( application
    , Api
    , ServerApi
    ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text, intercalate)
import Servant (Get, PlainText, (:<|>)(..), serve, Handler, hoistServer, (:>), ServerError)
import Servant.Server (Application, ServerT)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)

import qualified Hangman.Server.Games as Games
import Hangman.Application.Ports (GameMonad, PuzzleGeneratorMonad)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import Hangman.Read.Game (GameReadMonad)

cat :: Text
cat =
  intercalate "\n"
      [ " _._     _,-'\"\"`-._"
      , "(,-.`._,'(       |\\`-/|"
      , "    `-.-' \\ )-`( , o o)"
      , "          `-    \\`_`\"'-" ]

type Api =
    Get '[PlainText] Text
    :<|> "games" :> Games.Api

type ServerApi = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Api

server
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => ServerT ServerApi m
server = swaggerSchemaUIServerT (toSwagger @Api Proxy) :<|> return cat :<|> Games.api

application
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => (forall x. m x -> Handler x)
    -> Application
application runMonadStack = serve api $ hoistServer api runMonadStack server
  where
    api = Proxy @ServerApi
