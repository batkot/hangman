import qualified Hangman.Server.GamesSpec        as GamesSpec (tests)
import qualified Hangman.Server.GetSpec          as GetSpec (tests)
import           Hangman.Server.Resources.WebApp (withWebApp)
import           Test.Tasty                      (defaultMain, testGroup)

main :: IO ()
main = do
    defaultMain $ withWebApp $ \client ->
        testGroup "E2E tests"
            [ GetSpec.tests client
            , GamesSpec.tests client ]

