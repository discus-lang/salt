
module Salt.Main.Mode.Parse where
import Salt.Main.Mode.Lex
import Salt.Core.Exp
import Salt.Data.Location

import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Data.Pretty               as P

import qualified System.Exit                    as System
import qualified Text.Show.Pretty               as Show


-- | Parse a source file and print the result to stdout.
mainParse :: FilePath -> IO ()
mainParse filePath
 = do   mm      <- runParse filePath
        putStrLn $ Show.ppShow mm


-- | Load and parse a source module.
runParse  :: FilePath -> IO (Module RL)
runParse filePath
 = do   toks       <- runLex filePath
        let result = Parser.parseModule toks

        case result of
         Left errs
          -> do putStrLn $ P.render $ P.vcat
                         $ map (Parser.ppParseError filePath) errs
                System.exitFailure

         Right mm -> return mm

