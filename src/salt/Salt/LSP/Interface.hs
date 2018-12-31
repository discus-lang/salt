
module Salt.LSP.Interface where
import Salt.LSP.Protocol.Request
import Salt.LSP.Protocol.Base
import Salt.LSP.State
import qualified System.IO              as S
import qualified Text.JSON              as J
import qualified Text.Show.Pretty       as T
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T


---------------------------------------------------------------------------------------------------
-- | Read a JsonRPC message from stdin.
--   TODO: only trace this stuff if flag is enabled, it's too chatty.
lspRead :: forall a. (Show a, Unpack a) 
        => State -> IO (Request a)
lspRead state
 = do   lspLog state "* Waiting for message"
        txContentLength <- T.hGetLine S.stdin

        -- TODO: handle broken messages, don't just fail with pattern match.
        let Just txLength1  = T.stripPrefix "Content-Length: " txContentLength
        let Just txLength   = T.stripSuffix "\r" txLength1
        let lenChunk        = read (T.unpack txLength)

        "\r" <- T.hGetLine S.stdin
        txChunk  <- lspReadChunk state lenChunk

        case J.decode $ T.unpack txChunk of
         J.Error str 
          -> do lspLog state $ "  error: " ++ show str
                lspRead state
        
        -- TODO: check sequence number.        
         J.Ok js 
          -> case unpack js of
                Nothing  
                 -> do  lspLog state $ " error: jsonrpc malformed"
                        lspLog state $ T.ppShow js
                        lspRead state

                Just (req :: Request a )
                 -> do  -- TODO: trace on flag.
                        -- lspLog state $ T.ppShow req
                        return req


-- | Read a chunk of the given size from stdin.
lspReadChunk :: State -> Int -> IO T.Text
lspReadChunk _state nChunk
 = loop 0 BS.empty
 where
        loop nAcc bsAcc
         | nAcc >= nChunk    
         = return $ T.decodeUtf8 bsAcc

         | otherwise
         = do   let nRemain = max 1 (nChunk - nAcc)
                bsMoar    <- BS.hGet S.stdin nRemain 
                let nMoar =  BS.length bsMoar
                loop (nAcc + nMoar) (BS.append bsAcc bsMoar)


-- | Send a JSON value via JsonRPC to the client.
--   We print it to Stdout with the content-length headers.
lspSend :: State -> J.JSValue -> IO ()
lspSend _state js
 = do   let payload = J.encode js
        S.putStr $ "Content-Length: " ++ show (length payload) ++ "\r\n"
        S.putStr $ "\r\n"
        S.putStr payload
        S.hFlush S.stdout
