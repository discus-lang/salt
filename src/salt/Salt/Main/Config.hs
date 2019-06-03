
module Salt.Main.Config where
import Data.Text                (Text)
import qualified System.Exit    as System
import qualified Data.Text      as T


-- | Command line configuration.
data Config
        = Config
        { configMode    :: Maybe Mode }


-- | Command line mode.
data Mode
        = ModeLSP
        { modeFileLog   :: Maybe FilePath }

        | ModeEmit      FilePath

        | ModeTest      FilePath
        | ModeTest1     FilePath Text

        | ModeCheck     FilePath
        | ModeParse     FilePath
        | ModeLex       FilePath

        | ModeMake      FilePath
        deriving Show


-- | Default command line mode.
configDefault :: Config
configDefault
        = Config
        { configMode    = Nothing }


-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 = return config

parseArgs ("-lsp" : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeLSP Nothing) }

parseArgs ("-lsp-debug" : fileLog : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeLSP (Just fileLog)) }

parseArgs ("-emit" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeEmit filePath) }

parseArgs ("-test" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeTest filePath) }

parseArgs ("-test1"  : filePath : name : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeTest1 filePath (T.pack name)) }

parseArgs ("-check" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeCheck filePath) }

parseArgs ("-parse" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeParse filePath) }

parseArgs ("-lex" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeLex filePath) }

parseArgs (filePath : []) config
 = return
 $ config { configMode = Just (ModeMake filePath) }

parseArgs _ _
 = do   putStr usage
        System.exitFailure

usage
 = unlines
 [ "salt: The compilation target that functional programmers always wanted."
 , ""
 , " salt FILE.salt                   Emit result if one is define, else run tests."
 , " salt -emit   FILE.salt           Emit the defined result."
 , " salt -test   FILE.salt           Run all the tests in the given module."
 , " salt -test1  FILE.salt NAME      Run a single test in the given module."
 , " salt -check  FILE.salt           Type check a core file and print its AST."
 , " salt -parse  FILE.salt           Tarse a core file and print its AST."
 , " salt -lex    FILE.salt           Lex a core file and print its tokens."
 , ""
 , " salt -lsp                        Become a language server"
 , " salt -lsp-debug FILE.log          .. and log debug messages to a file"
 , ""
 ]
