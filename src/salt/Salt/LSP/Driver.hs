
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
 = case statePhase state of
        PhaseStartup
         -> do  msg <- lspRead state
                lspInitialize state msg

        PhaseInitialized
         -> do  msg <- lspRead state
                lspHandle state msg

        _ -> do
                lspLog state "not done yet"
                lspLoop state

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
lspInitialize :: State -> Request JSValue -> IO ()
lspInitialize state req

 -- Client sends us 'inititialize' with the set of its capabilities.
 -- We reply with our own capabilities.
 | "initialize" <- reqMethod req
 , Just (params :: InitializeParams) 
      <- join $ fmap unpack $ reqParams req
 = do  
       lspLog state "* Initialize"
       lspLog state $ T.ppShow params

       lspSend state 
        $ pack $ ResponseResult (reqId req)
        $ O [ ( "capabilities"
              , O [ ( "textDocumentSync"
                    , O [ ("openClose",  F $ pack True)        -- send us open/close notif to server.
                        , ("change",     F $ pack (3 :: Int))  -- send us incremental changes
                        , ("willSave",   F $ pack True)        -- send us will-save notif.
                        , ("save",       F $ pack True)        -- send us save notif.
                        ])])]

       lspLoop state

 -- Cient sends us 'initialized' if it it is happy with the 
 -- capabilities that we sent.
 | "initialized" <- reqMethod req
 = do  lspLog  state "* Initialized"
       lspLoop state { statePhase = PhaseInitialized }

 -- Something wen't wrong.
 | otherwise
 = do  lspLog  state "* Initialization received unexpected message."
       lspLoop state


---------------------------------------------------------------------------------------------------
lspHandle :: State -> Request JSValue -> IO ()
lspHandle state req
 = do
        lspLog  state "* Request"
        lspLog  state (T.ppShow req)
        lspLoop state 
