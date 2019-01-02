
module Salt.LSP.Task.Diagnostics where
import Salt.LSP.Diagnostic.Parser
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Core.Codec.Text.Lexer     as Lexer

import Data.Char


----------------------------------------------------------------------------------------- Update --  
-- | Compute diagnostics for a source file, and push them to the client.
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
                Left errs       
                 -> sendParserDiagnostics state sUri 
                 $  map (diagnosticOfParseError toks) errs

                Right _mm
                 -> sendClearDiagnostics state sUri


------------------------------------------------------------------------------------------ Clear --
-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendClearDiagnostics :: State -> String -> IO ()
sendClearDiagnostics state sUri
 = do   lspLog  state "* Clearing Diagnostics"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A [] ]]


------------------------------------------------------------------------------------------ Lexer --
-- | Send lexer errors to the client.
sendLexerErrors :: State -> String -> [Lexer.LexerError] -> IO ()
sendLexerErrors state sUri errs
 = do   lspLog  state "* Sending Lexer Errors"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A $ map (V . packLexerError) errs ]]


-- | Expand and pack a lexer error into JSON.
--
--   The errors we get from a lexer will only indicate the first character
--   that was not part of a valid token. In the editor window we prefer to
--   report the error location from that point until the next space character
--   or end of line, so they're easier to read.
--   
packLexerError :: Lexer.LexerError -> JSValue
packLexerError (Lexer.LexerError nLine nColStart csRest)
 = jobj [ "range"       := V $ packRange (Lexer.Range locStart locEnd)
        , "severity"    := I 1
        , "source"      := S "lexer"
        , "message"     := S "Lexical error." ]

 where  nColEnd 
         = expand nColStart csRest
 
        locStart = Lexer.Location nLine nColStart
        locEnd   = Lexer.Location nLine nColEnd

        expand n []     = n
        expand n (c : cs)
         | isSpace c    = n
         | c == '\n'    = n
         | otherwise    = expand (n + 1) cs


----------------------------------------------------------------------------------------- Parser --
sendParserDiagnostics :: State -> String -> [ParserDiagnostic] -> IO ()
sendParserDiagnostics state sUri diags
 = do   lspLog state "* Sending Parser Errors"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A (map (V . packParserDiagnostic) diags) ]]

packParserDiagnostic :: ParserDiagnostic -> JSValue
packParserDiagnostic (ParserDiagnostic range sMsg)
 = jobj [ "range"       := V $ packRange range
        , "severity"    := I 1
        , "source"      := S "parser"
        , "message"     := S sMsg ]

packRange :: Lexer.Range Lexer.Location -> JSValue
packRange (Lexer.Range locFirst locFinal)
 = jobj [ "start"       := V $ packLocation locFirst
        , "end"         := V $ packLocation locFinal]

packLocation :: Lexer.Location -> JSValue
packLocation (Lexer.Location nLine nCol)
 = jobj [ "line"        := J nLine 
        , "character"   := J nCol ]

       