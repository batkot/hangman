{-# LANGUAGE RecordWildCards #-}
module Hangman.Server.GetSpec
    ( tests
    ) where

import           Data.Either                     (isRight)
import           Hangman.Server.Resources.WebApp (WebClient (..))
import           Servant.Client                  (runClientM)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                (testCase, (@?))

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "GET / tests"
        [ testCase "Should return cat" $ do
            WebClient{..} <- webClientM
            response <- runClientM get env
            isRight response @? "Should get result"
        ]
