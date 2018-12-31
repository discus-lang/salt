
module Salt.LSP.Protocol.Request where
import Salt.LSP.Protocol.JsonRPC
import Salt.LSP.Protocol.Base


-- | Client request.
data Request
        = Request
        { requestId     :: Integer
        , requestMethod :: String
        , requestParams :: JSValue }
        deriving Show


instance Unpack Request where
 unpack js
  = do  rpc :: JsonRPC      <- unpack js
        guard $ jsonrpcVersion rpc == "2.0"
        let iid         = jsonrpcId     rpc
        let method      = jsonrpcMethod rpc
        let params      = jsonrpcParams rpc 
        return  $ Request iid method params
