{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Hangman.Server
    ( application
    , Api
    , ServerApi
    ) where

import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text, intercalate)
import           Servant                   (Get, Handler, PlainText, Raw,
                                            ServerError, hoistServer, serve,
                                            (:<|>) (..), (:>))
import           Servant.Server            (Application, ServerT)
import           Servant.Swagger           (toSwagger)
import           Servant.Swagger.UI        (SwaggerSchemaUI,
                                            swaggerSchemaUIServerT)

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Hangman.Application.Ports (GameMonad, PuzzleGeneratorMonad)
import           Hangman.Read.Game         (GameReadMonad)
import qualified Hangman.Server.Games      as Games
import qualified Web.Hyperbole             as Hyperbole

cat :: Text
cat =
  intercalate "\n"
      [ " _._     _,-'\"\"`-._"
      , "(,-.`._,'(       |\\`-/|"
      , "    `-.-' \\ )-`( , o o)"
      , "          `-    \\`_`\"'-" ]

type Api =
    Get '[PlainText] Text
    :<|> "ui" :> Raw
    :<|> "games" :> Games.Api

type ServerApi = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Api

server
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => Application
    -> ServerT ServerApi m
server app = swaggerSchemaUIServerT (toSwagger @Api Proxy) :<|> return cat :<|> pure app :<|> Games.api

application
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => (forall x. m x -> Handler x)
    -> Application
application runMonadStack = serve api $ hoistServer api runMonadStack $ server hyperApp
  where
    api = Proxy @ServerApi
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument "Hello") (Hyperbole.page testPage)
    testPage = Hyperbole.load $ pure $ Hyperbole.el Hyperbole.bold "Yo"
