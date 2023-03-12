import Hangman.Server.Resources.WebApp (withWebApp)
import qualified Hangman.Server.GetSpec as GetSpec (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    defaultMain $ withWebApp $ \client ->
        testGroup "E2E tests"
            [ GetSpec.tests client ]

