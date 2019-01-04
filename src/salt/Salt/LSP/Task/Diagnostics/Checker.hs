
module Salt.LSP.Task.Diagnostics.Checker where
import Salt.LSP.Task.Diagnostics.Lexer
import Salt.LSP.Task.Diagnostics.Base
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.Data.Location
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Data.Pretty               as P


------------------------------------------------------------------------------------------ Types --
-- | A type checker diagnostic to send to the client.
data CheckerDiagnostic
        = CheckerDiagnostic
        { checkerDiagnosticRange        :: Lexer.Range Lexer.Location
        , checkerDiagnosticMessage      :: String }
        deriving Show


------------------------------------------------------------------------------------------- Send --
-- | Send type checker errors to the client.
sendCheckerDiagnostics :: State -> String -> [CheckerDiagnostic] -> IO ()
sendCheckerDiagnostics state sUri diags
 = do   lspLog  state "* Sending Checker Diagnostics"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A (map (V . packCheckerDiagnostic) diags) ]]


-- | Pack a `CheckerDiagnostic` into JSON.
packCheckerDiagnostic :: CheckerDiagnostic -> JSValue
packCheckerDiagnostic (CheckerDiagnostic range sMsg)
 = jobj [ "range"       := V $ packRange range
        , "severity"    := I 1
        , "source"      := S "checker"
        , "message"     := S sMsg ]


------------------------------------------------------------------------------------------ Build --
-- | Build a diagnostic from a type checker error.
diagnosticOfCheckerError
        :: Check.Error (Range Location) -> CheckerDiagnostic
diagnosticOfCheckerError err
 = CheckerDiagnostic range' msg
 where
        range   = Check.errorAnnot err
        range'  = mungeRangeForVSCode range
        msg     = P.render $ P.ppr () err
