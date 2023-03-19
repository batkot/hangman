module Main (main) where

import Hangman.Server (application)
import Hangman.Adapters.InMemory (runConstPuzzleGenT, runInMemoryGameStorageT)

import Data.HashMap.Strict (empty)
import Data.List.NonEmpty (fromList)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (Parser, long, short, metavar, help, option, auto, (<**>), info, helper, fullDesc, progDesc, execParser)

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
    run (port opt) app
  where
    options = info (optionParser <**> helper)
        (fullDesc <> progDesc "Run Foo Server")
    app = application (runConstPuzzleGenT (fromList "PUZZLE") . runInMemoryGameStorageT empty)
