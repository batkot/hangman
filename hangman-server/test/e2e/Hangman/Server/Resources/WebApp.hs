{-# LANGUAGE TypeApplications #-}
module Hangman.Server.Resources.WebApp
    ( WebClient(..)
    , withWebApp
    , GamesClient(..)
    ) where

import Hangman.Server (application, Api)

import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, BaseUrl (..), Scheme (Http))
import Data.List.NonEmpty (fromList)
import Data.HashMap.Strict (empty)
import Test.Tasty (TestTree, withResource)
import Control.Concurrent (ThreadId, killThread, forkIO)
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.IORef (newIORef)
import Hangman.Adapters.InMemory
import Servant ((:<|>)(..))
import Hangman.Server.Games (CreateGameRequest(..), CreateGameResponse(..))

data WebClient = WebClient
    { get :: ClientM Text
    , games :: GamesClient
    , env :: ClientEnv
    }

data GamesClient = GamesClient
    { createGame :: CreateGameRequest -> ClientM CreateGameResponse
    , getGame :: Text -> ClientM CreateGameResponse
    , guessLetter :: Text -> Char -> ClientM CreateGameResponse
    }

newtype WebAppHandle = WebAppHandle { unHandle :: (Warp.Port, ThreadId) }

withWebApp :: (IO WebClient -> TestTree) -> TestTree
withWebApp tests =
    withResource createWebApp destroyWebApp $ \handle ->
        withResource (handle >>= createWebAppClient) (const (return ())) tests

createWebAppClient :: WebAppHandle -> IO WebClient
createWebAppClient (WebAppHandle (port, _)) = do
    let (app :<|> (createGame :<|> getGame :<|> guessLetter)) = client @Api Proxy
        baseUrl = BaseUrl Http "localhost" port ""
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
        gamesClient = GamesClient createGame getGame guessLetter
    return $ WebClient app gamesClient clientEnv

createWebApp :: IO WebAppHandle
createWebApp = do
    (port, socket) <- Warp.openFreePort
    gamesIoRef <- newIORef empty
    threadId <- forkIO $ Warp.runSettingsSocket Warp.defaultSettings socket $ application (runConstPuzzleGenT (fromList "PUZZLE") . runInMemoryGameStorageT gamesIoRef)
    return (WebAppHandle (port, threadId))

destroyWebApp :: WebAppHandle -> IO ()
destroyWebApp = killThread . snd . unHandle
