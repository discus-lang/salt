
module Salt.LSP.Task.CodeLens where
import Salt.LSP.Task.Diagnostics.Lexer
import Salt.LSP.Task.Diagnostics
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.LSP.State
import Salt.Core.Exp
import Salt.Data.Location
import Data.IORef
import Data.Maybe
import qualified Data.Map.Strict        as Map


updateCodeLenses :: State -> JsonRpcId -> String -> IO ()
updateCodeLenses state reqId sUri
 = goLookup True
 where
        goLookup bFirst
         = do   mp <- readIORef $ stateCoreChecked state
                case Map.lookup sUri mp of
                 -- We haven't seen this source file before.
                 Nothing
                  |  bFirst
                  -> do -- Call updateDiagnostics which will check the module and
                        -- add it to the state if it's ok.
                        updateDiagnostics state sUri sUri
                        goLookup False

                  | otherwise
                  ->    return ()

                 -- File has type errors, so cannot produce code lenses:
                 Just Nothing
                  -> sendCodeLenses state reqId []

                 -- Produce lenses for all the top-level test declarations.
                 Just (Just mm)
                  -> goLensMe mm

        goLensMe mm
         = do   let lenses = codeLensesOfModule mm
                sendCodeLenses state reqId lenses


---------------------------------------------------------------------------------------------------
sendCodeLenses :: State -> JsonRpcId -> [CodeLens] -> IO ()
sendCodeLenses state reqId codeLenses
 = do   lspLog  state "* Sending Code Lenses"
        lspSend state $ jobj
         [ "id"         := V $ pack reqId
         , "result"     := A (map (V . packCodeLens) codeLenses) ]

packCodeLens :: CodeLens -> JSValue
packCodeLens (CodeLens range (Command sTitle sCommand) _mData)
 = jobj [ "range"       := V $ packRange range
        , "command"
          := O  [ "title"       := S sTitle
                , "command"     := S sCommand ]]


---------------------------------------------------------------------------------------------------
data CodeLens
        = CodeLens
        { codeLensRange         :: Range Location
        , codeLensCommand       :: Command
        , codeLensData          :: Maybe JSValue }
        deriving (Eq, Show)

data Command
        = Command
        { commandTitle          :: String
        , commandCommand        :: String }
        deriving (Eq, Show)


codeLensesOfModule :: Module (Range Location) -> [CodeLens]
codeLensesOfModule mm
 = mapMaybe codeLensOfDecl $ moduleDecls mm


-- | Send code lens for each of the eval tests.
--   TODO: open a new file with the results.
codeLensOfDecl :: Decl (Range Location) -> Maybe CodeLens
codeLensOfDecl (DTest (DeclTestEvalTerm a _n _m))
 = Just $ CodeLens a
        (Command "test results" "extension.helloWorld") Nothing

codeLensOfDecl _ = Nothing