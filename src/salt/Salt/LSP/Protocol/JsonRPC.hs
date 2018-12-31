
module Salt.LSP.Protocol.JsonRPC where
import Salt.LSP.Protocol.Base

-- | JsonRPC message.
data JsonRPC 
        = JsonRPC 
        { jsonrpcVersion        :: String
        , jsonrpcId             :: Integer
        , jsonrpcMethod         :: String
        , jsonrpcParams         :: JSValue }
        deriving (Show)

instance Unpack JsonRPC where
 unpack js
  = do  sVersion <- getString  =<< getField js "jsonrpc"
        iid      <- getInteger =<< getField js "id" 
        sMethod  <- getString  =<< getField js "method"
        jParams  <- getField js "params"
        return $ JsonRPC sVersion iid sMethod jParams

