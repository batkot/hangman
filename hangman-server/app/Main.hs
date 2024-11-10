{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Hangman.Adapters.InMemory (runConstPuzzleGen,
                                            runGameEffectInMem,
                                            runGameReadEffectInMem)
import           Hangman.Server            (application)

import qualified Control.Monad.Except      as E
import           Control.Monad.IO.Class    (liftIO)
import           Data.HashMap.Strict       (empty)
import           Data.IORef                (newIORef)
import           Data.List.NonEmpty        (fromList)
import           Effectful                 (runEff)
import           Effectful.Error.Dynamic   (runErrorNoCallStack)
import           Network.Wai.Handler.Warp  (run)
import           Options.Applicative       (Parser, auto, execParser, fullDesc,
                                            help, helper, info, long, metavar,
                                            option, progDesc, short, (<**>))

data Options = Options
    { port :: Int
    }

optionParser :: Parser Options
optionParser = Options
    <$> option auto (long "port" <> short 'p' <> metavar "INT" <> help "Port to run http server")

main :: IO ()
main = do
    opt <- execParser options
    putStrLn $ "Starting on port " <> show (port opt)
    createApp >>= run (port opt)
  where
    options = info (optionParser <**> helper)
        (fullDesc <> progDesc "Run Hangman Server")
    createApp = do
       ioRef <- newIORef empty
       return $ application $ runEffectful ioRef
    runEffectful ioRef = runConstPuzzleGen (fromList "PUZZLE") . runGameReadEffectInMem ioRef . runGameEffectInMem ioRef
