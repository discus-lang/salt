
module Salt.LSP.Driver
       (runLSP)
where
import Salt.LSP.Protocol
import Salt.LSP.Interface
import Salt.LSP.State
import qualified Salt.LSP.Task.Diagnostics      as Task

import Data.IORef
import qualified System.IO                      as System
import qualified System.Exit                    as System
import qualified System.Posix.Process           as Process
import qualified Control.Exception              as Control
import qualified Data.Map                       as Map
import qualified Text.Show.Pretty               as T


---------------------------------------------------------------------------------------------------
-- | Become a language server plugin.
--
--   * We listen to requests on stdin and send responses to stdout.
--   * We take an optional path for server side logging.
--   * If the server process crashes then try to write the reason to the debug log.
--
runLSP :: Maybe FilePath -> IO ()
runLSP mFileLog
 = Control.catch
        -- Enter the main server loop.
        (do state  <- lspBegin mFileLog
            lspLoop state)

        -- If we get any exception from the server process then try to
        -- write it to the log file, if we have one.
        (\(e :: Control.SomeException)
         -> do  (case mFileLog of
                  Nothing  -> return ()
                  Just file
                   -> System.appendFile file
                        $  "\n" ++ T.ppShow e)

                System.die $ unlines
                 [ "salt lsp server crashed"
                 , T.ppShow e ])


---------------------------------------------------------------------------------------------------
-- | The main event loop for the language server.
--
--   We do a blocking read of stdin to get a request,
--   then dispatch it to the appropriat
lspLoop :: State -> IO ()
lspLoop state
 = case statePhase state of
        PhaseStartup
         -> do  msg <- lspRead state
                lspStartup state msg

        PhaseInitialized
         -> do  msg <- lspRead state
                lspInitialized state msg

        _ -> do
                lspLog state "not done yet"
                lspLoop state


---------------------------------------------------------------------------------------------------
-- | Begin the language server plugin.
--   We open our local file and initialize the state.
lspBegin :: Maybe FilePath -> IO State
lspBegin mFileLog
 = do
        pid <- Process.getProcessID

        -- Create a new file for the debug log, if we were asked for one.
        mLogDebug
         <- case mFileLog of
              Nothing -> return Nothing
              Just filePath
                -> do let filePathPid = filePath ++ "." ++ show pid
                      hLogDebug <- System.openFile filePathPid System.WriteMode
                      return  $ Just (filePathPid, hLogDebug)

        -- The type checked module is stored here, when we have one.
        refCoreChecked <- newIORef Map.empty

        -- The complete state.
        let state
                = State
                { stateLogDebug         = mLogDebug
                , statePhase            = PhaseStartup
                , stateCoreChecked      = refCoreChecked }

        lspLog state "* Salt language server starting up"
        return state


---------------------------------------------------------------------------------------------------
-- | Handle startup phase where we wait for the
--   initializatino request sent from the client.
lspStartup :: State -> Request JSValue -> IO ()
lspStartup state req

 -- Client sends us 'inititialize' with the set of its capabilities.
 -- We reply with our own capabilities.
 | "initialize" <- reqMethod req
 , Just (params :: InitializeParams)
      <- join $ fmap unpack $ reqParams req
 = do
        lspLog state "* Initialize"

        -- Log the list of client capabilities.
        lspLog state $ T.ppShow params

        -- Tell the client what our capabilities are.
        lspSend state $ jobj
         [ "id" := V $ pack $ reqId req
         , "result"
           := O  [ "capabilities"
                    := O  [ "textDocumentSync"
                            := O  [ "openClose" := B True   -- send us open/close notif.
                                  , "change"    := I 1      -- send us full file changes.
                                  , "save"      := B True   -- send us save notif.
                                  ]]]]
        lspLoop state

 -- Cient sends us 'initialized' if it it is happy with the
 -- capabilities that we sent.
 | "initialized" <- reqMethod req
 = do  lspLog  state "* Initialized"
       lspLoop state { statePhase = PhaseInitialized }

 -- Something went wrong.
 | otherwise
 = do  lspLog  state "* Initialization received unexpected message."
       lspLoop state


---------------------------------------------------------------------------------------------------
-- | Main event handler of the server.
--
--   Once initialized we receive the main requests and update our state.
--
lspInitialized :: State -> Request JSValue -> IO ()
lspInitialized state req

 -- On startup VSCode sends us a didChangeConfiguration,
 --   but we don't have any settings define, so the payload is empty.
 --   Just drop it on the floor.
 | "workspace/didChangeConfiguration" <- reqMethod req
 , Just jParams         <- reqParams req
 , Just jSettings       <- getField jParams   "settings"
 , Just jSettingsSalt   <- getField jSettings "salt"
 = do
        lspLog state "* DidChangeConfiguration (salt)"
        lspLog state $ "  jSettings:    " ++ show jSettingsSalt
        lspLoop state

 -- A file was opened.
 | "textDocument/didOpen" <- reqMethod req
 , Just jParams         <- reqParams req
 , Just jDoc            <- getField jParams "textDocument"
 , Just sUri            <- getString  =<< getField jDoc "uri"
 , Just sLanguageId     <- getString  =<< getField jDoc "languageId"
 , Just iVersion        <- getInteger =<< getField jDoc "version"
 , Just sText           <- getString  =<< getField jDoc "text"
 = do
        lspLog state "* DidOpen"
        lspLog state $ "  sUri:         " ++ show sUri
        lspLog state $ "  sLanguageId:  " ++ show sLanguageId
        lspLog state $ "  iVersion:     " ++ show iVersion
        lspLog state $ "  sText:        " ++ show sText

        Task.updateDiagnostics state sUri sText
        lspLoop state

 -- A file was closed.
 | "textDocument/didClose" <- reqMethod req
 , Just jParams         <- reqParams req
 , Just jDoc            <- getField jParams "textDocument"
 , Just sUri            <- getString =<< getField jDoc "uri"
 = do
        lspLog state "* DidClose"
        lspLog state $ "  sUri:         " ++ show sUri

        -- Once the file is closed, clear any errors that it might still have
        -- from the IDE.
        Task.sendClearDiagnostics state sUri
        lspLoop state

 -- A file was saved.
 | "textDocument/didSave" <- reqMethod req
 , Just jParams         <- reqParams req
 , Just jDoc            <- getField jParams "textDocument"
 , Just sUri            <- getString  =<< getField jDoc "uri"
 , Just iVersion        <- getInteger =<< getField jDoc "version"
 = do
        lspLog state "* DidSave"
        lspLog state $ "  sUri:         " ++ show sUri
        lspLog state $ "  iVersion:     " ++ show iVersion
        lspLoop state

 -- A file was changed.
 | "textDocument/didChange" <- reqMethod req
 , Just jParams         <- reqParams req
 , Just jDoc            <- getField jParams "textDocument"
 , Just sUri            <- getString  =<< getField jDoc "uri"
 , Just iVersion        <- getInteger =<< getField jDoc "version"
 , Just [jChange]       <- getArray   =<< getField jParams "contentChanges"
 , Just sText           <- getString  =<< getField jChange "text"
 = do
        lspLog state "* DidChange"
        lspLog state $ "  sUri:         " ++ show sUri
        lspLog state $ "  iVersion:     " ++ show iVersion
        lspLog state $ "  sText:        " ++ show sText
        Task.updateDiagnostics state sUri sText
        lspLoop state

 -- Some other request that we don't handle.
 | otherwise
 = do
        lspLog  state "* Request"
        lspLog  state (T.ppShow req)
        lspLoop state
