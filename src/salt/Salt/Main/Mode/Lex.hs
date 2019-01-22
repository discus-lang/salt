
module Salt.Main.Mode.Lex where
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Core.Codec.Text.Token     as Token

import Data.Function
import qualified Text.Lexer.Inchworm.Char       as IW
import qualified System.Exit                    as System


-- | Lex a source module and print the result to stdout.
mainLex :: FilePath -> IO ()
mainLex filePath
 = do   toks       <- runLex filePath
        putStr  $ unlines $ map show toks


-- | Lex a source file into tokens.
runLex :: FilePath -> IO [Token.At Token.Token]
runLex filePath
 = do   source  <- readFile filePath

        (toks, loc, strRest)
         <- IW.scanStringIO source Lexer.scanner

        let toks'  = [ Token.At l k
                     | Token.At l k <- toks
                     , k & \case Token.KMetaComment _ -> False
                                 _                    -> True]

        case strRest of
         [] -> return toks'
         _  -> System.die
                $ "lexical error at "
                ++ show loc
                ++ " " ++ show (take 10 strRest) ++ "..."
