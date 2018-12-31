
module Salt.LSP.Driver 
       (runLSP)
where
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.LSP.State
import qualified System.IO                as System
import qualified System.Posix.Process     as Process
import qualified Text.Show.Pretty         as T

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
 = do  msg <- lspRead state
       lspInitialize state msg
     
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
                      hLogDebug <- System.openFile filePathPid System.WriteMode
                      return  $ Just (filePathPid, hLogDebug)
       let state
                = State
                { stateLogDebug = mLogDebug
                , statePhase    = PhaseStartup }

       lspLog state "* Salt language server starting up"
       return state


---------------------------------------------------------------------------------------------------
-- | Handle the initialization request sent from the client.
lspInitialize :: State -> Request -> IO ()
lspInitialize state req
 | Just (params :: InitializeParams) <- unpack $ requestParams req
 = do  lspLog state "* Initialize"
       lspLog state $ T.ppShow params

       lspLoop state

 | otherwise
 = do  lspLog  state "* bad init message"
       lspLoop state

