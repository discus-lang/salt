
module Salt.LSP.Task.Diagnostics.Lexer where
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.Core.Codec.Text.Lexer
import Data.Char


-- | Send lexer errors to the client.
sendLexerErrors :: State -> String -> [LexerError] -> IO ()
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
packLexerError :: LexerError -> JSValue
packLexerError (LexerError nLine nColStart csRest)
 = jobj [ "range"       := V $ packRange (Range locStart locEnd)
        , "severity"    := I 1
        , "source"      := S "lexer"
        , "message"     := S "Lexical error." ]

 where  nColEnd 
         = expand nColStart csRest
 
        locStart = Location nLine nColStart
        locEnd   = Location nLine nColEnd

        expand n []     = n
        expand n (c : cs)
         | isSpace c    = n
         | c == '\n'    = n
         | otherwise    = expand (n + 1) cs


packRange :: Range Location -> JSValue
packRange (Range locFirst locFinal)
 = jobj [ "start"       := V $ packLocation locFirst
        , "end"         := V $ packLocation locFinal]


packLocation :: Location -> JSValue
packLocation (Location nLine nCol)
 = jobj [ "line"        := J nLine 
        , "character"   := J nCol ]
