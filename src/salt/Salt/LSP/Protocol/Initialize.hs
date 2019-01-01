
module Salt.LSP.Protocol.Initialize where
import Salt.LSP.Protocol.Base
import qualified Data.List      as List


-- | Request from the client to initialize the server.
data InitializeParams
        = InitializeParams
        { ipProcessId                   :: Maybe Integer
        , ipRootUri                     :: Maybe String
        , ipInitializationOptions       :: Maybe JSValue
        , ipCapabilities                :: [(String, JSValue)]
        , ipTrace                       :: Maybe String
        , ipWorkspaceFolders            :: Maybe JSValue
        }
        deriving Show


instance Unpack InitializeParams where
 unpack js
  = do  mProcessId             <- getIntegerNull =<< getField js "processId"
        mRootUri               <- getStringNull  =<< getField js "rootUri"
        let mInitOptions       =  getField js "initializationOptions"
        jCapabilities          <- getField js "capabilities"
        mTrace                 <- maybe (Just Nothing) (fmap Just getString) $ getField js "trace"
        let mjWorkspaceFolders =  getField js "workspaceFolders"
        
        return  $ InitializeParams 
                mProcessId mRootUri 
                mInitOptions
                [ (List.intercalate "." fs, v) 
                        | (fs, v) <- flattenJSValue jCapabilities]
                mTrace
                mjWorkspaceFolders

