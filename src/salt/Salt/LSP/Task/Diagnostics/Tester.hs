
module Salt.LSP.Task.Diagnostics.Tester where
import Salt.LSP.Task.Diagnostics.Lexer
import Salt.LSP.Task.Diagnostics.Base
import Salt.LSP.State
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.Core.Codec.Text                     ()

import Salt.Core.Exp
import qualified Salt.Core.Check                as Check
import qualified Salt.Core.Eval                 as Eval
import qualified Salt.Core.Codec.Text.Lexer     as Lexer
import qualified Salt.Data.Pretty               as P

import Data.Maybe
import qualified Data.Text                      as T

----------------------------------------------------------------------------------------- Types --
-- | A test diagnostic to send to the client.
data TesterDiagnostic
        = TesterDiagnosticWatch
        { testerDiagnosticSource        :: String
        , testerDiagnosticRange         :: Lexer.Range Lexer.Location
        , testerDiagnosticSeverity      :: Severity
        , testerDiagnosticMessage       :: String }
        deriving Show


------------------------------------------------------------------------------------------- Send --
-- | Send type checker errors to the client.
sendTesterDiagnostics :: State -> String -> [TesterDiagnostic] -> IO ()
sendTesterDiagnostics state sUri diags
 = do   lspLog  state "* Sending Tester Diagnostics"
        lspSend state $ jobj
         [ "method" := S "textDocument/publishDiagnostics"
         , "params" := O [ "uri"         := S sUri
                         , "diagnostics" := A (map (V . packTesterDiagnostic) diags) ]]


-- | Pack a `TesterDiagnostic` into JSON.
packTesterDiagnostic :: TesterDiagnostic -> JSValue
packTesterDiagnostic (TesterDiagnosticWatch sSource range severity sMsg)
 = jobj [ "range"       := V $ packRange range
        , "severity"    := J (codeOfSeverity severity)
        , "source"      := S ("test " ++ sSource)
        , "message"     := S sMsg ]


------------------------------------------------------------------------------------------ Build --
-- | Build test diagnostics.

type RL = Lexer.Range Lexer.Location
rlNone  = Lexer.Range (Lexer.Location 0 0) (Lexer.Location 0 0)

buildTesterDiagnostics
        :: State
        -> Check.Context RL
        -> Module RL
        -> IO [TesterDiagnostic]

buildTesterDiagnostics _state ctx mm
 = fmap catMaybes
 $ mapM (buildTesterDiagnosticOfDecl ctx mm) $ moduleDecls mm


buildTesterDiagnosticOfDecl
        :: Check.Context RL
        -> Module RL
        -> Decl   RL
        -> IO (Maybe TesterDiagnostic)

-- test kind
buildTesterDiagnosticOfDecl ctx _mm
        (DTest (DeclTestKind aRange bWatch mName tTest))
 | bWatch
 = do   let aRange' = mungeRangeForVSCode $ fromMaybe aRange $ takeAnnotOfType tTest
        (_tTest, kResult) <- Check.checkType aRange [] ctx tTest
        let sResult = slurpTestIdent mName ++ (P.render $ P.ppr () kResult)
        return $ Just $ TesterDiagnosticWatch "kind" aRange' SeverityInformation sResult

-- test type
buildTesterDiagnosticOfDecl ctx _mm
        (DTest (DeclTestType aRange bWatch mName mTest))
 | bWatch
 = do   let aRange' = mungeRangeForVSCode $ fromMaybe aRange $ takeAnnotOfTerm mTest
        (_mTest, tResult, _effs) <- Check.synthTermProductive aRange [] ctx mTest
        let sResult = slurpTestIdent mName ++ (P.render $ P.ppr () tResult)
        return $ Just $ TesterDiagnosticWatch "type" aRange' SeverityInformation sResult

-- test eval'type
buildTesterDiagnosticOfDecl _ctx mm
        (DTest (DeclTestEvalType aRange bWatch mName tTest))
 | bWatch
 = do   state       <- Eval.newState Eval.configDefault mm
        let aRange'  = mungeRangeForVSCode $ fromMaybe aRange $ takeAnnotOfType tTest
        tResult     <- Eval.evalType state aRange' (TypeEnv []) tTest
        let sResult  = slurpTestIdent mName ++ (P.render $ P.ppr () tResult)
        return  $ Just $ TesterDiagnosticWatch "eval'type" aRange' SeverityInformation sResult

-- test eval'term
buildTesterDiagnosticOfDecl _ctx mm
        (DTest (DeclTestEvalTerm aRange bWatch mName mTest))
 | bWatch
 = do   state       <- Eval.newState Eval.configDefault mm
        let aRange'  = mungeRangeForVSCode $ fromMaybe aRange $ takeAnnotOfTerm mTest
        vsResult    <- Eval.evalTerm state aRange' (TermEnv []) mTest
        let sResult  = slurpTestIdent mName ++ (P.render $ P.ppr () vsResult)
        return  $ Just $ TesterDiagnosticWatch "eval" aRange' SeverityInformation sResult

-- test assert
buildTesterDiagnosticOfDecl _ctx mm
        (DTest (DeclTestAssert aRange bWatch mName mTest))
 | bWatch
 = do   state     <- Eval.newState Eval.configDefault mm
        let aRange' = mungeRangeForVSCode $ fromMaybe aRange $ takeAnnotOfTerm mTest
        vsResult  <- Eval.evalTerm state aRange' (TermEnv []) mTest
        let (sev, sDiag :: String)
                = case vsResult of
                        [VTrue]  -> (SeverityInformation, "ok")
                        [VFalse] -> (SeverityError,   "failed")
                        _        -> (SeverityError,  "invalid")

        let sResult  = slurpTestIdent mName ++ sDiag

        return  $ Just $ TesterDiagnosticWatch "assert" aRange' sev sResult

buildTesterDiagnosticOfDecl _ _ _
 = return $ Nothing


slurpTestIdent :: Maybe Name -> String
slurpTestIdent mn
 = case mn of
        Nothing         -> ""
        Just (Name tx)  -> T.unpack tx ++ " = "


