{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Hangman.Server.Resources.WebApp
    ( WebClient(..)
    , withWebApp
    , GamesClient(..)
    ) where

import           Hangman.Server            (application)
import           Hangman.Server.API        (Api)

import           Control.Concurrent        (ThreadId, forkIO, killThread)
import qualified Control.Monad.Except      as E
import           Control.Monad.IO.Class    (liftIO)
import           Data.HashMap.Strict       (empty)
import           Data.IORef                (newIORef)
import           Data.List.NonEmpty        (fromList)
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import           Effectful                 (runEff)
import           Effectful.Error.Dynamic   (runErrorNoCallStack)
import           Hangman.Adapters.InMemory
import           Hangman.Server.API.Games  (CreateGameRequest (..),
                                            GameDescriptionResponse (..))
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant                   ((:<|>) (..))
import           Servant.Client            (BaseUrl (..), ClientEnv, ClientM,
                                            Scheme (Http), client, mkClientEnv)
import           Test.Tasty                (TestTree, withResource)

data WebClient = WebClient
    { games :: GamesClient
    , env   :: ClientEnv
    }

data GamesClient = GamesClient
    { createGame  :: CreateGameRequest -> ClientM GameDescriptionResponse
    , getGame     :: Text -> ClientM GameDescriptionResponse
    , guessLetter :: Text -> Char -> ClientM GameDescriptionResponse
    }

newtype WebAppHandle = WebAppHandle { unHandle :: (Warp.Port, ThreadId) }

withWebApp :: (IO WebClient -> TestTree) -> TestTree
withWebApp tests =
    withResource createWebApp destroyWebApp $ \handle ->
        withResource (handle >>= createWebAppClient) (const (return ())) tests

createWebAppClient :: WebAppHandle -> IO WebClient
createWebAppClient (WebAppHandle (port, _)) = do
    let (createGame :<|> getGame :<|> guessLetter) = client @Api Proxy
        baseUrl = BaseUrl Http "localhost" port ""
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
        gamesClient = GamesClient createGame getGame guessLetter
    return $ WebClient gamesClient clientEnv

createWebApp :: IO WebAppHandle
createWebApp = do
    (port, socket) <- Warp.openFreePort
    gamesIoRef <- newIORef empty
    threadId <- forkIO $ Warp.runSettingsSocket Warp.defaultSettings socket $ application $ runEffectful gamesIoRef
    return (WebAppHandle (port, threadId))
  where
    runEffectful ioRef = runConstPuzzleGen (fromList "PUZZLE"). runGameReadEffectInMem ioRef . runGameEffectInMem ioRef

destroyWebApp :: WebAppHandle -> IO ()
destroyWebApp = killThread . snd . unHandle
