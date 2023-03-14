{-# LANGUAGE RecordWildCards #-}
module Hangman.Server.GetSpec
    ( tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Hangman.Server.Resources.WebApp (WebClient(..))
import Test.Tasty.HUnit (testCase, (@?))
import Servant.Client (runClientM)
import Data.Either (isRight)

tests :: IO WebClient -> TestTree
tests webClientM =
    testGroup "GET / tests"
        [ testCase "Should return cat" $ do
            WebClient{..} <- webClientM
            response <- runClientM get env
            isRight response @? "Should get result"
        ]
