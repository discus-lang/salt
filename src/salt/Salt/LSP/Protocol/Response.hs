
module Salt.LSP.Protocol.Response where
import Salt.LSP.Protocol.Base


---------------------------------------------------------------------------------------------------
data Response a
        = ResponseResult
        { rspId                 :: JsonRpcId 
        , rspResult             :: a }

        | ResponseError
        { rspId                 :: JsonRpcId 
        , rspErrorCode          :: ResponseErrorCode
        , rspErrorMessage       :: String
        , rspErrorData          :: Maybe JSValue }
        deriving Show


instance Pack a => Pack (Response a) where
 pack (ResponseResult jid xResult) 
  = jobj [ "id"          := V $ pack jid
         , "result"      := V $ pack xResult ]

 pack (ResponseError jid errCode sMsg mValue)
  = jobj [ "id"          := V $ pack jid
         , "error"       
           := O [ "code"        := V $ pack errCode
                , "message"     := V $ pack sMsg
                , "data"        ?= fmap V mValue ]]


---------------------------------------------------------------------------------------------------
data ResponseErrorCode
        = RecParseError                 -- -32700
        | RecInvalidRequest             -- -32600
        | RecMethodNotFound             -- -32601
        | RecInvalidParams              -- -32602
        | RecInternalError              -- -32606
        | RecServerErrorStart           -- -32099
        | RecServerErrorEnd             -- -32000
        | RecServerNotInitialized       -- -32002
        | RecUnknownErrorCode           -- -32001
        | RecRequestCancelled           -- -32800
        deriving Show

instance Pack ResponseErrorCode where
 pack = \case
        RecParseError           -> pack (-32700 :: Int)
        RecInvalidRequest       -> pack (-32600 :: Int)
        RecMethodNotFound       -> pack (-32601 :: Int)
        RecInvalidParams        -> pack (-32602 :: Int)
        RecInternalError        -> pack (-32603 :: Int)
        RecServerErrorStart     -> pack (-32099 :: Int)
        RecServerErrorEnd       -> pack (-32000 :: Int)
        RecServerNotInitialized -> pack (-32002 :: Int)
        RecUnknownErrorCode     -> pack (-32001 :: Int)
        RecRequestCancelled     -> pack (-32800 :: Int)
