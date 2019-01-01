
module Salt.LSP.Task.Diagnostics where
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Data.Char as Char


---------------------------------------------------------------------------------------------------
updateDiagnostics :: State -> String -> String -> IO ()
updateDiagnostics state sUri sSource 
 = goLex
 where  
        goLex 
         = case Lexer.lexSource sSource of
                Left errs       -> sendLexerErrors  state sUri errs    
                Right toks      -> goParse toks
         
        goParse toks
         = case Parser.parseModule toks of
                Left errs       -> sendParserErrors state sUri errs
                Right _mm       -> sendClearDiagnostics state sUri


---------------------------------------------------------------------------------------------------
-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendClearDiagnostics :: State -> String -> IO ()
sendClearDiagnostics state sUri
 = do   lspLog  state "* Clearing Diagnostics"
        lspSend state
         $ pack $ Notification "textDocument/publishDiagnostics"
         $ Just $ pack
         $ O    [ "uri"         := S sUri
                , "diagnostics" := A [] ]


---------------------------------------------------------------------------------------------------
-- | Send lexer errors to the client.
sendLexerErrors :: State -> String -> [Lexer.LexerError] -> IO ()
sendLexerErrors state sUri errs
 = do   lspLog  state "* Sending Lexer Errors"
        lspSend state
         $ pack $ Notification "textDocument/publishDiagnostics"
         $ Just $ pack 
         $ O    [ "uri"         := S sUri
                , "diagnostics" := A $ map (V . packLexerError) errs ]


-- | Expand and pack a lexer error into JSON.
--
--   The errors we get from the lexer only indicate the first character
--   that was not part of a valid token. In the editor window we prefer
--   to report the error location from that point until the next space
--   character or end of line, so they're easier to read.
--   
packLexerError :: Lexer.LexerError -> JSValue
packLexerError (Lexer.LexerError nLine nColStart csRest)
 = pack $ O
        [ "range"       := O    [ "start" := V $ packLocation locStart
                                , "end"   := V $ packLocation locEnd ]
        , "severity"    := I 1
        , "source"      := S "lexer"
        , "message"     := S "lexical error" ]

 where  nColEnd 
         = expand nColStart csRest
 
        locStart = Lexer.Location (nLine - 1) (nColStart - 1)
        locEnd   = Lexer.Location (nLine - 1) (nColEnd   - 1)

        expand n []             = n
        expand n (c : cs)
         | Char.isSpace c       = n
         | c == '\n'            = n
         | otherwise            = expand (n + 1) cs


---------------------------------------------------------------------------------------------------
-- | Send parser errors to the client.
sendParserErrors :: State -> String -> [Parser.ParseError] -> IO ()
sendParserErrors state sUri errs
 = do   lspLog state "* Sending Parser Errors"
        lspSend state
         $ pack $ O
                [ "method" := S "textDocument/publishDiagnostics"
                , "params"      
                  := O  [ "uri"         := S sUri
                        , "diagnostics" := A (map (V . packParserError) errs) ]]


packParserError :: Parser.ParseError -> JSValue
packParserError (Parser.ParseError locStart locEnd _msgs)
 = pack $ O
        [ "range"       := O    [ "start" := V $ packLocation locStart
                                , "end"   := V $ packLocation locEnd]
        , "severity"    := I 1
        , "source"      := S "parser"
        , "message"     := S "parse error" ]

packLocation :: Lexer.Location -> JSValue
packLocation (Lexer.Location nLine nCol)
 = pack $ O
        [ "line"        := J $ nLine - 1
        , "character"   := J $ nCol  - 1 ]



        
