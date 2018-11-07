
import War.Main.Config
import War.Main.Option
import BuildBox
import System.Environment
import System.Directory
import qualified War.Task.Test              as T


main :: IO ()
main
 = do   -- Parse command line options, and exit if they're no good.
        args    <- getArgs
        config  <- parseOptions args defaultConfig
        let Just spec = configTest config
        mainTest spec


-- | Run tests from the provided directories
mainTest :: T.Spec -> IO ()
mainTest spec
 = do   tmp     <- getTemporaryDirectory
        result  <- runBuild tmp $ T.build spec
        case result of
         Left err       -> error    $ show err
         Right _        -> return ()

