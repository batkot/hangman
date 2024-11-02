{-# LANGUAGE OverloadedStrings #-}
module Hangman.Server.UI (ui, UI) where

import           Servant        (Raw)
import           Servant.Server (ServerT)

import qualified Web.Hyperbole  as Hyperbole

type UI = Raw

ui :: ServerT UI m
ui =
    pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument "Hello") (Hyperbole.page testPage)
    testPage = Hyperbole.load $ pure $ Hyperbole.el Hyperbole.bold "Yo"
