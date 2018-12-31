
module Salt.LSP.Driver 
       (runLSP)
where
import Salt.LSP.Interface
import Salt.LSP.State
import qualified System.IO                as S
import qualified System.Posix.Process     as Process

---------------------------------------------------------------------------------------------------
-- | Become a language server plugin,
--   listening to requests on stdin and sending responses to stdout.
--   We take an optional path for server side logging.
runLSP :: Maybe FilePath -> IO ()
runLSP mFileLog
 = do  state  <- lspStartup mFileLog
       lspLoop state


---------------------------------------------------------------------------------------------------
lspLoop :: State -> IO ()
lspLoop state
 | PhaseStartup <- statePhase state
 = do  msg    <- lspRead state
       lspLog state (show msg)
       lspLoop state
     
 | otherwise
 = error "not done yet"


---------------------------------------------------------------------------------------------------
-- | Startup the language server plugin.
--   We open our local file and initialize the state.
lspStartup :: Maybe FilePath -> IO State
lspStartup mFileLog
 = do
       pid <- Process.getProcessID 
       mLogDebug
        <- case mFileLog of
              Nothing -> return Nothing
              Just filePath
                -> do let filePathPid = filePath ++ "." ++ show pid
                      hLogDebug <- S.openFile filePathPid S.WriteMode
                      return  $ Just (filePathPid, hLogDebug)
       let state
                = State
                { stateLogDebug = mLogDebug
                , statePhase    = PhaseStartup }

       lspLog state "* Salt language server starting up"
       return state

