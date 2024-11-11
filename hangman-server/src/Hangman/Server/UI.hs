{-# OPTIONS_GHC -Wno-orphans #-}
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

import           Servant                         (Raw)
import           Servant.Server                  (ServerT)

import           Control.Monad                   (forM_)
import           Control.Monad.Identity          (Identity)
import           Data.Bifunctor                  (second)
import           Data.Function                   ((&))
import qualified Data.List.NonEmpty              as NE
import           Data.String                     (IsString)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Tuple.Extra                (dupe)
import qualified Data.UUID                       as UUID
import           Effectful                       (Eff, IOE, (:>))
import           Effectful.Error.Dynamic         (runErrorNoCallStackWith)
import           GHC.Generics                    (Generic)
import           Hangman.Application.CreateGame  (createGame)
import           Hangman.Application.GuessLetter (GuessLetterError (..),
                                                  guessLetter)
import           Hangman.Application.Ports       (GameEffect,
                                                  PuzzleGeneratorEffect)
import           Hangman.Model.Game              (GameId (..), GameState (..))
import qualified Hangman.Model.PositiveInt       as PositiveInt
import           Hangman.Named
import           Hangman.Read.Game               (GameDescription,
                                                  GameReadEffect, chancesLeft,
                                                  findGameDescription, puzzle,
                                                  state)
import qualified Web.Hyperbole                   as Hyperbole
import           Web.Hyperbole.Forms             (Field)
import           Web.Hyperbole.Route             (Route (..))
import qualified Web.View.Style                  as WView

type UI = Raw

data AppRoute
  = Landing
  | NewGame
  | Play GameId
  deriving stock (Eq,Generic)
  deriving anyclass (Route)

instance Route GameId where
  defRoute = GameId $ UUID.fromWords 0 0 0 0

  matchRoute [segment] = GameId <$> UUID.fromText segment
  matchRoute _         = Nothing

  routePath (GameId gameId) = pure $ UUID.toText gameId

type RunEff = forall es x. IOE :> es => Eff (GameEffect : GameReadEffect : PuzzleGeneratorEffect : es) x -> Eff es x

ui :: RunEff -> ServerT UI m
ui runEff = pure hyperApp
  where
    hyperApp = Hyperbole.liveApp (Hyperbole.basicDocument hangmanLabel) (runEff $ Hyperbole.routeRequest router)
    router :: GameReadEffect :> es => GameEffect :> es => Hyperbole.Hyperbole :> es => AppRoute -> Hyperbole.Eff es Hyperbole.Response
    router Landing = Hyperbole.page $ Hyperbole.load @_ @'[] $ pure landingPage
    router NewGame = Hyperbole.page createGamePage
    router (Play gameId) = Hyperbole.page $ playGamePage gameId

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
      let ten = foldr ($) PositiveInt.one $ replicate 9 PositiveInt.increment
      newGameId <- createGame ten $ NE.fromList $ T.unpack $ solution createGameForm
      Hyperbole.redirect $ Hyperbole.routeUrl $ Play newGameId
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
        (WView.cls "font"
          & WView.prop "font-size" ("3rem" :: Text))

data PlayGame = PlayGame
  deriving stock (Read, Show)
  deriving anyclass (Hyperbole.ViewId)

newtype PlayGameAction = Guess Char
  deriving stock (Read, Show)
  deriving anyclass (Hyperbole.ViewAction)

instance Hyperbole.HyperView PlayGame where
  type Action PlayGame = PlayGameAction

playGamePage :: GameEffect :> es => GameReadEffect :> es => Hyperbole.Hyperbole :> es => GameId -> Hyperbole.Page es '[PlayGame]
playGamePage gameId = do
  Hyperbole.handle (playGame gameId) $ Hyperbole.load $ do
    name gameId $ \gId -> do
      gameDescription <- findGameDescription gId
      pure $ Hyperbole.hyper PlayGame $
          pageContainer $ maybe "Not found" playGameView gameDescription

playGame :: GameReadEffect :> es => GameEffect :> es => Hyperbole.Hyperbole :> es => GameId -> PlayGame -> PlayGameAction -> Eff es (Hyperbole.View PlayGame ())
playGame gameId PlayGame (Guess char) = do
  runErrorNoCallStackWith (\(GameNotFound _) -> pure ()) $ guessLetter gameId char
  name gameId $ \gId -> do
    gameDescription <- findGameDescription gId
    pure $ pageContainer $ Hyperbole.el_ $ maybe "Not found" playGameView gameDescription

playGameView :: GameDescription gameId -> Hyperbole.View PlayGame ()
playGameView gameDescription = do
  Hyperbole.el_ $ Hyperbole.text $ maybe gameEnding (T.pack . show . PositiveInt.toInt) $ chancesLeft gameDescription
  Hyperbole.el_ $ Hyperbole.text $ puzzle gameDescription
  forM_ ['A'..'Z'] keyboardBtn
  where
    gameEnding =
      case state gameDescription of
        Won  -> "WON"
        Lost -> "DEAD"
        _    -> ""
    keyboardBtn char = Hyperbole.button (Guess char) keyboardStyle $ Hyperbole.text $ T.pack [char]
    keyboardStyle =
      WView.addClass
        (WView.cls "keyboard-btn"
          & WView.prop "font-size" ("2rem" :: Text)
          & WView.prop "min-width" ("40px" :: Text))
      . WView.hover (WView.bg ("000" :: Hyperbole.HexColor) . WView.color ("FFF" :: Hyperbole.HexColor))

pageContainer :: Hyperbole.View c () -> Hyperbole.View c ()
pageContainer page = do
  Hyperbole.el containerStyle $ Hyperbole.el logoContainer page
  where
    containerStyle =
      WView.addClass
        $ WView.cls "container"
          & WView.prop "width" ("100vw" :: Text)
          & WView.prop "min-height" ("100vh" :: Text)
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
