
module Salt.LSP.Protocol.Initialize where
import Salt.LSP.Protocol.Base
import qualified Data.List      as List


-- | Request from the client to initialize the server.
data InitializeParams
        = InitializeParams
        { ipProcessId                   :: Maybe Integer
        , ipRootUri                     :: Maybe String
        , ipClientCapabilities          :: [(String, JSValue)]
        , ipTrace                       :: Maybe String
        }
        deriving Show


instance Unpack InitializeParams where
 unpack js
  = do  mProcessId      <- getIntegerNull =<< getField js "processId"
        mRootUri        <- getStringNull  =<< getField js "rootUri"
        capabilities    <- getField js "capabilities"
        mTrace          <- maybe (Just Nothing) (fmap Just getString) $ getField js "trace"
        
        return  $ InitializeParams 
                mProcessId   mRootUri 
                [ (List.intercalate "." fs, v) 
                        | (fs, v) <- flattenJSValue capabilities]
                mTrace

