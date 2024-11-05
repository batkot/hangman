{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hangman.Server.UI (ui, UI) where

import           Servant        (Raw)
import           Servant.Server (ServerT)

import           Data.Function  ((&))
import           Data.String    (IsString)
import           Data.Text      (Text)
import           GHC.Generics   (Generic)
import qualified Web.Hyperbole  as Hyperbole
import           Web.Hyperbole  (Route (..))
import qualified Web.View.Style as WView

type UI = Raw

data AppRoute
  = Landing
  | NewGame
  deriving (Eq,Generic)

instance Route AppRoute where

ui :: ServerT UI m
ui = pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument hangmanLabel) (Hyperbole.routeRequest router)
    landing = Hyperbole.load $ pure landingPage
    test = Hyperbole.load $ pure $ do
      pageContainer $ Hyperbole.el_ "Game placeholder!"

    -- router :: AppRoute -> Hyperbole.Eff fx Hyperbole.Response
    router Landing = Hyperbole.page landing
    router NewGame = Hyperbole.page test


landingPage :: Hyperbole.View c ()
landingPage = pageContainer $ do
    Hyperbole.el_ hangmanLabel
    Hyperbole.route NewGame id "Random"

pageContainer :: Hyperbole.View c () -> Hyperbole.View c ()
pageContainer page = do
  Hyperbole.el containerStyle $ Hyperbole.el logoContainer page
  where
    containerStyle =
      WView.addClass
        $ WView.cls "container"
          & WView.prop "width" ("100vh" :: Text)
          & WView.prop "height" ("100vh" :: Text)
    logoContainer =
      WView.addClass
        $ WView.cls "page"
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
