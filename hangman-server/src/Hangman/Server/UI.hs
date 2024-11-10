{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Hangman.Server.UI (ui, UI) where

import           Servant                        (Raw)
import           Servant.Server                 (ServerT)

import           Control.Monad.Identity         (Identity)
import           Data.Bifunctor                 (second)
import           Data.Function                  ((&))
import qualified Data.List.NonEmpty             as NE
import           Data.String                    (IsString)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Tuple.Extra               (dupe)
import           Data.UUID                      (toText)
import           Effectful                      (Eff, IOE, (:>))
import           GHC.Generics                   (Generic)
import           Hangman.Application.CreateGame (createGame)
import           Hangman.Application.Ports      (GameEffect,
                                                 PuzzleGeneratorEffect)
import           Hangman.Model.Game             (GameId (..))
import qualified Hangman.Model.PositiveInt      as PositiveInt
import           Hangman.Read.Game              (GameReadEffect)
import qualified Web.Hyperbole                  as Hyperbole
import           Web.Hyperbole                  (Route (..))
import           Web.Hyperbole.Forms            (Field)
import qualified Web.View.Style                 as WView

type UI = Raw

data AppRoute
  = Landing
  | NewGame
  deriving stock (Eq,Generic)
  deriving anyclass (Route)

type RunEff = forall es x. IOE :> es => Eff (GameEffect : GameReadEffect : PuzzleGeneratorEffect : es) x -> Eff es x

ui :: RunEff -> ServerT UI m
ui runEff = pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument hangmanLabel) (runEff $ Hyperbole.routeRequest router)
    router :: GameEffect :> es => Hyperbole.Hyperbole :> es => AppRoute -> Hyperbole.Eff es Hyperbole.Response
    router Landing = Hyperbole.page $ Hyperbole.load @_ @'[] $ pure landingPage
    router NewGame = Hyperbole.page createGamePage

landingPage :: Hyperbole.View c ()
landingPage = pageContainer $ do
    Hyperbole.el_ hangmanLabel
    Hyperbole.route NewGame linkButtonStyle "New Game"
    Hyperbole.link "/swagger-ui" linkButtonStyle "API"

linkButtonStyle :: Hyperbole.Mod
linkButtonStyle =
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

createGameAction :: GameEffect :> es => Hyperbole.Hyperbole :> es => CreateGameView -> CreateGameAction -> Eff es (Hyperbole.View CreateGameView ())
createGameAction _ CreateGame = do
  (createGameForm, formValidation) <- second validateForm . dupe <$> Hyperbole.formData @CreateGameForm

  if Hyperbole.anyInvalid formValidation
    then pure $ createGameFormView formValidation
    else do
      GameId newGameId <- createGame PositiveInt.one $ NE.fromList $ T.unpack $ solution createGameForm
      pure $ Hyperbole.text $ "Created game: " <> toText newGameId
  where
    validateForm :: CreateGameForm Identity -> CreateGameForm Hyperbole.Validated
    validateForm (CreateGameForm solution) =
      CreateGameForm $ Hyperbole.validate (length (T.words solution) /= 1) "Puzzle has to be a single word"

createGamePage :: GameEffect :> es => Hyperbole.Hyperbole :> es => Hyperbole.Page es '[CreateGameView]
createGamePage = do
  Hyperbole.handle createGameAction $ Hyperbole.load $ pure $ pageContainer $ Hyperbole.hyper CreateGameView (createGameFormView Hyperbole.genForm)

createGameFormView :: CreateGameForm Hyperbole.Validated -> Hyperbole.View CreateGameView ()
createGameFormView x = do
  let f = Hyperbole.formFieldsWith x
  Hyperbole.form @CreateGameForm CreateGame fontSize $ do
    Hyperbole.field (solution f) (const id) $ do
      Hyperbole.input Hyperbole.TextInput (centerText . Hyperbole.placeholder "Enter solution to puzzle")
      Hyperbole.el validationError Hyperbole.invalidText
    Hyperbole.submit linkButtonStyle "Create game"
  where
    centerText =
      WView.addClass
        (WView.cls "center"
          & WView.prop "text-align" ("center" :: Text))
    validationError =
      WView.addClass
        (WView.cls "error"
          & WView.prop "font-size" ("1rem" :: Text)
          & WView.prop "color" ("#f77" :: Text))
    fontSize =
      WView.addClass
        (WView.cls "link"
          & WView.prop "font-size" ("3rem" :: Text))

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
