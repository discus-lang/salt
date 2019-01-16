
module Salt.LSP.State where
import Salt.Core.Exp
import Salt.Data.Location

import Data.Map                 (Map)
import Data.IORef
import qualified System.Exit    as System
import qualified System.IO      as System


-- | Language server plugin state.
data State
        = State
        { statePhase            :: Phase
        , stateLogDebug         :: Maybe (FilePath, System.Handle)

          -- | Checked core files.
        , stateCoreChecked      :: IORef (Map String (Maybe (Module (Range Location)))) }


-- | Phase of the LSP server protocol.
data Phase
        -- | We have just started up and have not yet initialized with the client.
        = PhaseStartup

        -- | Initialization with the client failed.
        | PhaseInitFailed

        -- | We have initialized with the client and are now handling requests.
        | PhaseInitialized
        deriving (Eq, Show)


-- | Append a messgage to the server side log file,  if we have one.
lspLog :: State -> String -> IO ()
lspLog state str
 | Just (_, h)  <- stateLogDebug state
 = do   System.hPutStr h (str ++ "\n")
        System.hFlush h

 | otherwise = return ()


-- | Append a message to the server side log file and exit the process.
lspFail :: State -> String -> IO a
lspFail state str
 = do   lspLog state str
        System.die str
