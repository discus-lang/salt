
module War.Main.Config where
import qualified War.Task.Test          as T


-- | Configuration information read from command line arguments.
data Config
        = Config
        { -- | Whether to emit debugging info for war.
          configDebug   :: Bool

          -- | Config for test mode.
        , configTest    :: Maybe T.Spec }
        deriving Show


-- | Default configuration.
defaultConfig :: Config
defaultConfig
        = Config
        { configDebug   = False
        , configTest    = Just defaultTestSpec }


-- | Default tester configuration.
defaultTestSpec :: T.Spec
defaultTestSpec
        = T.Spec
        { T.specTestDirs                = []
        , T.specWays                    = []
        , T.specThreads                 = 1
        , T.specFormatPathWidth         = 80
        , T.specInteractive             = True
        , T.specResultsFileAll          = Nothing
        , T.specResultsFileFailed       = Nothing }


