{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Hangman.Server
    ( application
    ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text, intercalate)
import Servant (Get, PlainText, (:<|>)(..), Server, serve)
import Servant.Server (Application)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

cat :: Text
cat =
  intercalate "\n" [ " _._     _,-'\"\"`-._", "(,-.`._,'(       |\\`-/|", "    `-.-' \\ )-`( , o o)", "          `-    \\`_`\"'-" ]

type Api = Get '[PlainText] Text

type ServerApi = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Api

server :: Server ServerApi
server = swaggerSchemaUIServer (toSwagger @Api Proxy) :<|> return cat

application :: Application
application = serve @ServerApi Proxy server
