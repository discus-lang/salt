
module Salt.LSP.Interface where
import Salt.LSP.Protocol.Request
import Salt.LSP.Protocol.Base
import Salt.LSP.State
import qualified Text.JSON              as J
import qualified System.IO              as S
import qualified Text.Show.Pretty       as T
import qualified Data.Text              as T
import qualified Data.Text.IO           as T


---------------------------------------------------------------------------------------------------
-- | Read a JsonRPC message from stdin.
--   TODO: only trace this stuff if flag is enabled, it's too chatty.
lspRead :: State -> IO Request
lspRead state
 = do   lspLog state "* Waiting for message"

        txContentLength <- T.hGetLine S.stdin

        -- TODO: handle broken messages, don't just fail with pattern match.
        let Just txLength1  = T.stripPrefix "Content-Length: " txContentLength
        let Just txLength   = T.stripSuffix "\r" txLength1
        let lenChunk        = read (T.unpack txLength)

        "\r" <- T.hGetLine S.stdin
        txChunk  <- lspReadChunk state lenChunk ""
        case J.decode $ T.unpack txChunk of
         J.Error str 
          -> do lspLog state $ "  error: " ++ show str
                lspRead state

         J.Ok js 
          -> do lspLog state $ "> Received Message ---------------------------------"
                lspLog state $ T.ppShow js
                
                -- TODO: check sequence number.
                case unpack js of
                 Nothing  
                  -> do lspLog state $ " error: jsonrpc malformed"
                        lspRead state

                 Just (req :: Request)
                  -> do -- TODO: trace on flag.
                        lspLog state $ T.ppShow req
                        return req


-- | Read a chunk of the given size from stdin.
lspReadChunk :: State -> Int -> T.Text -> IO T.Text
lspReadChunk state n acc
 | T.length acc >= n = return acc
 | otherwise
 = do  moar   <- T.hGetChunk S.stdin
       lspReadChunk state n (T.append acc moar)


-- | Send a JSON value via JsonRPC to the client.
--   We print it to Stdout with the content-length headers.
lspSend :: J.JSValue -> IO ()
lspSend js
 = do   let payload = J.encode js
        S.putStr $ "Content-Length: " ++ show (length payload) ++ "\r\n"
        S.putStr $ "\r\n"
        S.putStr payload
        S.hFlush S.stdout


