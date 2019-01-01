
module Salt.LSP.Task.Diagnostics where
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Data.Char as Char


updateDiagnostics :: State -> String -> String -> IO ()
updateDiagnostics state sUri sSource 
 = goLex
 where  
        goLex 
         = case Lexer.lexSource sSource of
                Right _toks -> sendClearDiagnostics state sUri
                Left errs   -> sendLexerErrors  state sUri errs    
         
{-                let toks'  = [ Token.At l k
                     | Token.At l k <- toks
                     , k & \case Token.KComment _ -> False
                                 _                -> True]
-}


-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendClearDiagnostics :: State -> String -> IO ()
sendClearDiagnostics state sUri
 = do   lspLog  state "* Clearing Diagnostics"
        lspSend state
         $ pack $ Notification "textDocument/publishDiagnostics"
         $ Just $ pack
         $ O    [ ("uri",         F $ pack sUri)
                , ("diagnostics", F $ pack $ A []) ]


-- | Send lexer errors to the client.
sendLexerErrors :: State -> String -> [Lexer.LexerError] -> IO ()
sendLexerErrors state sUri errs
 = do   lspLog  state "* Sending Diagnostics"
        lspSend state
         $ pack $ Notification "textDocument/publishDiagnostics"
         $ Just $ pack 
         $ O    [ ("uri",         F $ pack sUri)
                , ("diagnostics", F $ pack $ A $ map packLexerError errs) ]


-- | Expand and pack a lexer error into JSON.
--
--   The errors we get from the lexer only indicate the first character
--   that was not part of a valid token. In the editor window we prefer
--   to report the error location from that point until the next space
--   character or end of line, so they're easier to read.
--   
packLexerError :: Lexer.LexerError -> JSValue
packLexerError (Lexer.LexerError nLine nColStart csRest)
 = pack 
 $ O    [ ( "range",    F $ pack 
                        $ O [ ("start", O [ ("line",      F $ pack (nLine     - 1 :: Int))
                                          , ("character", F $ pack (nColStart - 1 :: Int))])
                            , ("end",   O [ ("line",      F $ pack (nLine     - 1 :: Int))
                                          , ("character", F $ pack (nColEnd   - 1 :: Int))])])
        , ( "severity", F $ pack (1 :: Int))
        , ( "source",   F $ pack $ S "lexer")
        , ( "message",  F $ pack $ S "lexical error") ]

 where  nColEnd 
         = expand nColStart csRest
 
        expand n []             = n
        expand n (c : cs)
         | Char.isSpace c       = n
         | c == '\n'            = n
         | otherwise            = expand (n + 1) cs
