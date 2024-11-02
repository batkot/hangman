{-# LANGUAGE OverloadedStrings #-}
module Hangman.Server.UI (ui, UI) where

import           Servant        (Raw)
import           Servant.Server (ServerT)

import           Data.Function  ((&))
import           Data.String    (IsString)
import           Data.Text      (Text)
import qualified Web.Hyperbole  as Hyperbole
import qualified Web.View.Style as WView

type UI = Raw

ui :: ServerT UI m
ui = pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument hangmanLabel) (Hyperbole.page testPage)
    testPage = Hyperbole.load $ pure landingPage

landingPage :: Hyperbole.View c ()
landingPage = do
  Hyperbole.el containerStyle $ Hyperbole.el logoContainer hangmanLabel
  where
    containerStyle =
      WView.addClass
        $ WView.cls "container"
          & WView.prop "width" ("100vh" :: Text)
          & WView.prop "height" ("100vh" :: Text)
    logoContainer =
      WView.addClass
        $ WView.cls "logo"
          & WView.prop "margin" (0 :: Int)
          & WView.prop "padding" (0 :: Int)
          & WView.prop "top" ("50%" :: Text)
          & WView.prop "left" ("50%" :: Text)
          & WView.prop "position" ("absolute" :: Text)
          & WView.prop "transform" ("translate(-50%, -50%)" :: Text)
          & WView.prop "text-align" ("center" :: Text)
          & WView.prop "font-size" ("6rem" :: Text)

hangmanLabel :: IsString s => s
hangmanLabel = "H_NGM_N"
