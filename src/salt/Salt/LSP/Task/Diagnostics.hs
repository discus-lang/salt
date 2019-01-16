
module Salt.LSP.Task.Diagnostics where
import Salt.LSP.Task.Diagnostics.Lexer
import Salt.LSP.Task.Diagnostics.Parser
import Salt.LSP.Task.Diagnostics.Checker
import Salt.LSP.Task.Diagnostics.Tester
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import qualified Salt.Core.Codec.Text.Parser    as Parser
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Core.Codec.Text.Token     as Token
import qualified Salt.Core.Check                as Checker

import Data.IORef
import qualified Data.Map.Strict                as Map


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

                Right mm
                 -> goCheck toks mm

        goCheck toks mm
         = Checker.checkModule (Token.rangeOfTokenList toks) mm
         >>= \case
                Left errs
                 -> do  modifyIORef' (stateCoreChecked state)
                         $ \mp -> Map.delete sUri mp
                        sendCheckerDiagnostics state sUri
                         $  map diagnosticOfCheckerError errs

                Right (mm', ctx)
                 -> do  modifyIORef' (stateCoreChecked state)
                         $ \mp -> Map.insert sUri (Just mm') mp
                        goTest ctx mm'

        goTest ctx mm'
         = do   diags <- buildTesterDiagnostics state ctx mm'
                if (not $ null diags)
                 then sendTesterDiagnostics state sUri diags
                 else sendClearDiagnostics  state sUri



-- | Clear diagnostics for the given file.
--   We do this when we haven't found any problems with it.
sendClearDiagnostics :: State -> String -> IO ()
sendClearDiagnostics state sUri
 = do   lspLog  state "* Clearing Diagnostics"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A [] ]]
