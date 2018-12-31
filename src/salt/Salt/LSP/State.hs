
module Salt.LSP.State where
import qualified System.IO      as S


-- | Language server plugin state.
data State
        = State
        { statePhase    :: Phase 
        , stateLogDebug :: Maybe (FilePath, S.Handle) }


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
 = do   S.hPutStr h (str ++ "\n")
        S.hFlush h

 | otherwise = return ()
