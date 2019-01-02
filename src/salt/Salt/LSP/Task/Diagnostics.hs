
module Salt.LSP.Task.Diagnostics where
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Core.Codec.Text.Lexer     as Lexer

import Data.Maybe
import Data.List
import Data.Char
import qualified Text.Parsec.Error              as Parsec


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
                Left errs       -> sendParserErrors state sUri errs
                Right _mm       -> sendClearDiagnostics state sUri


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
sendParserErrors :: State -> String -> [Parser.ParseError] -> IO ()
sendParserErrors state sUri errs
 = do   lspLog state "* Sending Parser Errors"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A (map (V . packParserError) errs) ]]


-- TODO: we want source ranges on tokens, not just starting positions.
-- when entering a new decl the next token is usually a top level 
-- hard keyword like 'test', 'term', 'type' etc. if we seee this as the next token then
-- inhibit the "unexpected" part of the error message. We really have an incomplete
-- declaration, and the next token isn't unexpected by the programmer.
-- Set the location of the problem just after the last token, not on the 'test' token
-- that triggered the error.

packParserError :: Parser.ParseError -> JSValue
packParserError (Parser.ParseError range mLocPrev msgs)
 = jobj [ "range"       := V $ packRange range
        , "severity"    := I 1
        , "source"      := S "parser"
        , "message"     := S sMsg ]

 where  
        sMsg     
         = case catMaybes [mUnexpected, mSysUnexpect, mExpect, mMessage] of
                []      -> "Parse error."
                parts   -> intercalate "\n" parts ++ show mLocPrev
         
        mUnexpected     = listToMaybe   [ "Unexpected " ++ s ++ "."
                                        | Parsec.UnExpect s <- msgs ]

        mSysUnexpect    = listToMaybe   [ "Unexpected " ++ s ++ "."
                                        | Parsec.SysUnExpect s <- msgs ]

        mExpect         = listToMaybe   [ "Expecting " ++ s  ++ "."
                                        | Parsec.Expect s <- msgs ]

        mMessage        = listToMaybe   [ s | Parsec.Message s <- msgs ]


------------------------------------------------------------------------------------------- Pack --
packRange :: Lexer.Range Lexer.Location -> JSValue
packRange (Lexer.Range locFirst locFinal)
 = jobj [ "start"       := V $ packLocation locFirst
        , "end"         := V $ packLocation locFinal]

packLocation :: Lexer.Location -> JSValue
packLocation (Lexer.Location nLine nCol)
 = jobj [ "line"        := J nLine 
        , "character"   := J nCol ]

       