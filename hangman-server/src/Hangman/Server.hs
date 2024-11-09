{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Hangman.Server
    ( application
    ) where

import           Data.Proxy                (Proxy (..))
import           Servant                   (Handler, ServerError, hoistServer,
                                            serve, (:<|>) (..))
import           Servant.Server            (Application, ServerT)

import           Effectful                 (Eff, IOE, (:>))
import           Effectful.Error.Dynamic
import           Hangman.Application.Ports (GameEffect, PuzzleGeneratorEffect)
import           Hangman.Read.Game         (GameReadEffect)
import qualified Hangman.Server.API        as API
import qualified Hangman.Server.UI         as UI

type HangmanApi = API.ApiSwag :<|> UI.UI

server
    :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => IOE :> es
    => Error ServerError :> es
    => ServerT HangmanApi (Eff es)
server = API.api :<|> UI.ui

application
    :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => IOE :> es
    => Error ServerError :> es
    => (forall x. Eff es x -> Handler x)
    -> Application
application runMonadStack = serve api $ hoistServer api runMonadStack server
  where
    api = Proxy @HangmanApi

