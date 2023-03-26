{-# LANGUAGE RecordWildCards #-}
module Hangman.Server.GetSpec
    ( tests
    ) where

import           Data.ByteString.Lazy            (fromStrict)
import           Data.Text.Encoding              (encodeUtf8)
import           Hangman.Server.Resources.WebApp (WebClient (..))
import           Servant.Client                  (runClientM)
import           System.FilePath                 ((</>))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.Golden               (goldenVsString)

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "GET / tests"
        [ goldenVsString "Should return cat" (testDataDir </> "get-cat.golden") $ do
            WebClient{..} <- webClientM
            Right response <- runClientM get env
            return . fromStrict . encodeUtf8 $ response
        ]

testDataDir :: FilePath
testDataDir = "test" </> "e2e" </> "Hangman" </> "Server" </> "testdata"
