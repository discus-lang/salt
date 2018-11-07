
module War.Main.Option where
import War.Main.Config
import War.Task.Create.Way
import War.Task.Test                    as T
import Data.Char


-- | Parse command-line options.
parseOptions :: [String] -> Config -> IO Config
parseOptions [] _
 = do   printUsage Nothing
        error "Nothing to do..."

parseOptions args _
 | elem "-help" args || elem "--help" args
 = do   printUsage Nothing
        error "Nothing to do..."

parseOptions args0 config0
 = return $ eat args0 config0
 where
  eat [] config = config
  eat args@(arg : rest) config
        | elem arg ["-d", "-debug"]
        = eat rest $ config { configDebug = True }

        | otherwise
        = case eatt args defaultTestSpec of
           Left  badArg  -> error $ "Invalid argument " ++ show badArg
           Right tspec   -> config { configTest    = Just tspec }


  -- Parse options for test mode
  eatt [] spec = Right spec
  eatt args@(arg : rest) spec
        | elem arg ["-b", "-batch"]
        = eatt rest $ spec { T.specInteractive = False }

        | "-j" : sThreads : more     <- args
        , all isDigit sThreads
        = eatt more $ spec { T.specThreads       = read sThreads}

        | "-results" : file : more <- args
        = eatt more $ spec { T.specResultsFileAll    = Just file }

        | "-results-failed" : file : more <- args
        = eatt more $ spec { T.specResultsFileFailed = Just file }

        | "+compway" : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eatt more $ spec { T.specWays = T.specWays spec ++ [Way name wayFlags []] }

        | "+runway"  : name : flags  <- args
        , (wayFlags, more)           <- break (\x -> take 1 x == "+") flags
        = eatt more $ spec { T.specWays = T.specWays spec ++ [Way name [] wayFlags] }

        | '-' : _       <- arg
        = Left arg

        -- Accept dirs for test mode
        | otherwise
        = eatt rest  $ spec { specTestDirs  = specTestDirs spec ++ [arg]}


printUsage :: Maybe String -> IO ()
printUsage badArg
   = putStr
   $ unlines
   $ maybe [] (\arg -> ["invalid argument " ++ arg]) badArg
   ++   [ "Usage: war [flags]"
        , "  -help                         Display this help."
        , "  -debug, -d                    Emit debugging info for the war test driver."
        , ""
        , " Test mode: war <TESTDIR> ..."
        , "  -batch, -b                    Don't interactively ask what to do if a test fails."
        , "  -j <INT>                      Set number of threads (jobs) to use."
        , "  -results        <FILE>        Log test results to this file."
        , "  -results-failed <FILE>        Log failed tests to this file."
        , "  +compway <NAME> [OPTIONS]     Also compile with these DDC options."
        , "  +runway  <NAME> [OPTIONS]     Also run executables with these RTS options."
        , ""
        ]

