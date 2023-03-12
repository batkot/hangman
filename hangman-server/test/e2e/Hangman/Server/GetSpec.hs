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
            webClient <- webClientM
            response <- runClientM (get webClient) (env webClient)
            isRight response @? "Should get result"
        ]
