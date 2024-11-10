{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hangman.Server.UI (ui, UI) where

import           Servant        (Raw)
import           Servant.Server (ServerT)

import           Data.Function  ((&))
import           Data.String    (IsString)
import           Data.Text      (Text)
import           Effectful      ((:>), Eff)
import           GHC.Generics   (Generic)
import qualified Web.Hyperbole  as Hyperbole
import           Web.Hyperbole  (Route (..))
import qualified Web.View.Style as WView
import Web.Hyperbole.Forms (Field)

type UI = Raw

data AppRoute
  = Landing
  | NewGame
  deriving stock (Eq,Generic)
  deriving anyclass (Route)

ui :: ServerT UI m
ui = pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument hangmanLabel) (Hyperbole.routeRequest router)
    router :: Hyperbole.Hyperbole :> es => AppRoute -> Hyperbole.Eff es Hyperbole.Response
    router Landing = Hyperbole.page $ Hyperbole.load @_ @'[] $ pure landingPage
    router NewGame = Hyperbole.page createGamePage

landingPage :: Hyperbole.View c ()
landingPage = pageContainer $ do
    Hyperbole.el_ hangmanLabel
    Hyperbole.route NewGame landingLink "New Game"
    Hyperbole.link "/swagger-ui" landingLink "API"
  where
    landingLink =
      WView.addClass
        (WView.cls "link"
          & WView.prop "font-size" ("3rem" :: Text)
          & WView.prop "display" ("block" :: Text))
      . WView.hover (WView.bg ("000" :: Hyperbole.HexColor) . WView.color ("FFF" :: Hyperbole.HexColor))

data CreateGameView = CreateGameView
  deriving stock (Show, Read)
  deriving anyclass (Hyperbole.ViewId)

data CreateGameAction = CreateGame
  deriving stock (Show, Read)
  deriving anyclass (Hyperbole.ViewAction)

instance Hyperbole.HyperView CreateGameView where
  type Action CreateGameView = CreateGameAction

newtype CreateGameForm f = CreateGameForm { solution :: Field f Text }
  deriving (Generic)

instance Hyperbole.Form CreateGameForm Hyperbole.Validated

createGameAction :: Hyperbole.Hyperbole :> es => CreateGameView -> CreateGameAction -> Eff es (Hyperbole.View CreateGameView ())
createGameAction _ CreateGame = do
  createGameForm <- Hyperbole.formData @CreateGameForm
  pure $ Hyperbole.el_ "submitted"

createGamePage :: Hyperbole.Hyperbole :> es => Hyperbole.Page es '[CreateGameView]
createGamePage = do
  Hyperbole.handle createGameAction $ Hyperbole.load $ pure $ pageContainer $ Hyperbole.hyper CreateGameView (foo Hyperbole.genForm)
  where
    foo :: CreateGameForm Hyperbole.Validated -> Hyperbole.View CreateGameView ()
    foo x = do
      let f = Hyperbole.formFieldsWith x
      Hyperbole.form @CreateGameForm CreateGame id $ do
        Hyperbole.field (solution f) (const id) $ do
          Hyperbole.label "Solution"
          Hyperbole.input Hyperbole.Username (Hyperbole.placeholder "username")
        Hyperbole.submit id "Create game"

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
