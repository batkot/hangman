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
import           Servant                   (ServerError, hoistServer, serve,
                                            (:<|>) (..))
import           Servant.Server            (Application, ServerT)

import qualified Control.Monad.Except      as E
import           Control.Monad.IO.Class    (liftIO)
import           Effectful                 (Eff, IOE, runEff, (:>))
import           Effectful.Error.Dynamic
import           Hangman.Application.Ports (GameEffect, PuzzleGeneratorEffect)
import           Hangman.Read.Game         (GameReadEffect)
import qualified Hangman.Server.API        as API
import qualified Hangman.Server.UI         as UI

type HangmanApi = API.ApiSwag :<|> UI.UI

type RunEff = forall es x. IOE :> es => Eff (GameEffect : GameReadEffect : PuzzleGeneratorEffect : es) x -> Eff es x

server
    :: GameEffect :> es
    => GameReadEffect :> es
    => PuzzleGeneratorEffect :> es
    => Error ServerError :> es
    => RunEff
    -> ServerT HangmanApi (Eff es)
server runEffs = API.api :<|> UI.ui runEffs

application :: RunEff -> Application
application runEffs = serve api $ hoistServer api runEffectful $ server runEffs
  where
    api = Proxy @HangmanApi
    runEffectful eff = do
      res <- liftIO . runEff . runErrorNoCallStack . runEffs $ eff
      E.liftEither res
