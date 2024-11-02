{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Hangman.Server.API (api, Api, ApiSwag) where


import           Data.Proxy                (Proxy (..))
import           Servant                   (ServerError, (:<|>) (..), (:>))
import           Servant.Server            (ServerT)
import           Servant.Swagger           (toSwagger)
import           Servant.Swagger.UI        (SwaggerSchemaUI,
                                            swaggerSchemaUIServerT)

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Hangman.Application.Ports (GameMonad, PuzzleGeneratorMonad)
import           Hangman.Read.Game         (GameReadMonad)

import qualified Hangman.Server.API.Games  as Games

type Api = "games" :> Games.Api
type ApiSwag = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Api

api
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => ServerT ApiSwag m
api = swaggerSchemaUIServerT (toSwagger @Api Proxy) :<|> Games.api
