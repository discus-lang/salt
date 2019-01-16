
module Salt.LSP.Interface where
import Salt.LSP.Protocol.Request
import Salt.LSP.Protocol.Base
import Salt.LSP.State
import qualified System.IO              as S
import qualified Text.JSON              as J
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import qualified Text.Read              as T


-- | Read a JsonRPC message from stdin.
lspRead :: forall a. (Show a, Unpack a)
        => State -> IO (Request a)
lspRead state
 = do   lspLog state "* Waiting for message"
        txContentLength <- T.hGetLine S.stdin

        txLength1
         <- case T.stripPrefix "Content-Length: " txContentLength of
                Just tx -> return tx
                Nothing -> lspFail state "Invalid JsonRPC header from client: no Content-Length"

        txLength
         <- case T.stripSuffix "\r" txLength1 of
                Just tx -> return tx
                Nothing -> lspFail state "Invalid JsonRPC header from client: no CR after length"

        lenChunk
         <- case T.readMaybe (T.unpack txLength) of
                Just n  -> return n
                Nothing -> lspFail state "Invalid JsonRPC header from client: bad length"

        sCR     <- T.hGetLine S.stdin
        (case sCR of
                "\r"    -> return ()
                _       -> lspFail state "Invalid JsonRPC header from client: no CR")

        txChunk  <- lspReadChunk state lenChunk

        case J.decode $ T.unpack txChunk of
         J.Error str
          -> do lspLog state $ "  error: " ++ show str
                lspRead state

         J.Ok js
          -> case unpack js of
                Nothing
                 -> do  lspLog state $ " error: jsonrpc malformed"
                        lspLog state $ show txChunk
                        lspRead state

                Just (req :: Request a )
                 -> do  lspLog state $ show txChunk
                        return req


-- | Read a chunk of the given size from stdin.
---
--   Careful: the Content-Length needs to be the length of the utf8
--   encoded bytestring, not the number of unicode characters.
--   If this is wrong then the stream will get out of sync.
--
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
---
--   Careful: the Content-Length needs to be the length of the utf8
--   encoded bytestring, not the number of unicode characters.
--   If this is wrong then the stream will get out of sync.
--
lspSend :: State -> J.JSValue -> IO ()
lspSend _state js
 = do   let str = J.encode js
        let bs  = T.encodeUtf8 $ T.pack str
        S.putStr $ "Content-Length: " ++ show (BS.length bs) ++ "\r\n"
        S.putStr $ "\r\n"
        BS.hPut  S.stdout bs
        S.hFlush S.stdout

