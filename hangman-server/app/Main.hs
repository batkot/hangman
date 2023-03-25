module Main (main) where

import           Hangman.Adapters.InMemory (runConstPuzzleGenT,
                                            runInMemoryGameStorageT)
import           Hangman.Server            (application)

import           Data.HashMap.Strict       (empty)
import           Data.IORef                (newIORef)
import           Data.List.NonEmpty        (fromList)
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
        (fullDesc <> progDesc "Run Foo Server")
    createApp = do
       ioRef <- newIORef empty
       return $ application (runConstPuzzleGenT (fromList "PUZZLE") . runInMemoryGameStorageT ioRef)
