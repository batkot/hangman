{-# LANGUAGE TypeApplications #-}
module Hangman.Server.Resources.WebApp
    ( WebClient(..)
    , withWebApp
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

data WebClient = WebClient
    { get :: ClientM Text
    , env :: ClientEnv
    }

newtype WebAppHandle = WebAppHandle { unHandle :: (Warp.Port, ThreadId) }

withWebApp :: (IO WebClient -> TestTree) -> TestTree
withWebApp tests =
    withResource createWebApp destroyWebApp $ \handle ->
        withResource (handle >>= createWebAppClient) (const (return ())) tests

createWebAppClient :: WebAppHandle -> IO WebClient
createWebAppClient (WebAppHandle (port, _)) = do
    let (app :<|> _) = client @Api Proxy
        baseUrl = BaseUrl Http "localhost" port ""
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    return $ WebClient app clientEnv

createWebApp :: IO WebAppHandle
createWebApp = do
    (port, socket) <- Warp.openFreePort
    gamesIoRef <- newIORef empty
    threadId <- forkIO $ Warp.runSettingsSocket Warp.defaultSettings socket $ application (runConstPuzzleGenT (fromList "PUZZLE") . runInMemoryGameStorageT gamesIoRef)
    return (WebAppHandle (port, threadId))

destroyWebApp :: WebAppHandle -> IO ()
destroyWebApp = killThread . snd . unHandle
