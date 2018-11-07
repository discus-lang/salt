
module Salt.Main.Config where
import Data.Text                (Text)
import qualified System.Exit    as System
import qualified Data.Text      as T


-- | Command line mode.
data Mode
        = ModeLex       FilePath
        | ModeParse     FilePath
        | ModeCheck     FilePath
        | ModeTest      FilePath
        | ModeTest1     FilePath Text
        deriving Show

data Config
        = Config
        { configMode    :: Maybe Mode }


configDefault :: Config
configDefault
        = Config
        { configMode    = Nothing }


-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
 = return config

parseArgs ("-lex" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeLex filePath) }

parseArgs ("-parse" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeParse filePath) }

parseArgs ("-check" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeCheck filePath) }

parseArgs ("-test" : filePath : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeTest filePath) }

parseArgs ("-test1"  : filePath : name : rest) config
 = parseArgs rest
 $ config { configMode = Just (ModeTest1 filePath (T.pack name)) }

parseArgs (filePath : []) config
 = return
 $ config { configMode = Just (ModeTest filePath) }

parseArgs _ _
 = do   putStr usage
        System.exitFailure

usage
 = unlines
 [ "salt FLAGS"
 , "   -lex   FILE.salt         lex a core file and print its tokens"
 , "   -parse FILE.salt         parse a core file and print its AST"
 , "   -check FILE.salt         type check a core file and print its AST"
 , "   -test  FILE.salt         run all the tests in the given module"
 , "   -test1 FILE.salt NAME    run a single test in the given module"
 ]
