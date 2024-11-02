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

import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO)
import           Hangman.Application.Ports (GameMonad, PuzzleGeneratorMonad)
import           Hangman.Read.Game         (GameReadMonad)
import qualified Hangman.Server.API        as API
import qualified Hangman.Server.UI         as UI

type HangmanApi = API.ApiSwag :<|> UI.UI

server
    :: GameMonad m
    => GameReadMonad m
    => PuzzleGeneratorMonad m
    => MonadIO m
    => MonadError ServerError m
    => ServerT HangmanApi m
server = API.api :<|> UI.ui

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
    api = Proxy @HangmanApi

