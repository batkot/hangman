module Hangman.Server
    ( someFunc
    ) where

import Hangman.Rules (someFunc2)

someFunc :: IO ()
someFunc = putStrLn someFunc2
