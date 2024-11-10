{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Hangman.Server.API (api, Api, ApiSwag) where


import           Data.Proxy                (Proxy (..))
import qualified Servant                   as S
import           Servant                   (ServerError, (:<|>) (..))
import           Servant.Server            (ServerT)
import           Servant.Swagger           (toSwagger)
import           Servant.Swagger.UI        (SwaggerSchemaUI,
                                            swaggerSchemaUIServerT)

import           Hangman.Application.Ports (GameEffect, PuzzleGeneratorEffect)
import           Hangman.Read.Game         (GameReadEffect)

import           Effectful                 (Eff, IOE, (:>))
import           Effectful.Error.Dynamic   (Error)
import qualified Hangman.Server.API.Games  as Games

type Api = "games" S.:> Games.Api
type ApiSwag = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Api

api
    :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => Error ServerError :> es
    => ServerT ApiSwag (Eff es)
api = swaggerSchemaUIServerT (toSwagger @Api Proxy) :<|> Games.api
