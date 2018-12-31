
module Salt.LSP.Interface where
import Salt.LSP.State
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Text.JSON      as J
import qualified System.IO      as S
        

-- | Read a JsonRPC message from stdin.
lspRead :: State -> IO T.Text
lspRead state
 = do   lspLog state $ ". (waiting for message)"
        txContentLength <- T.hGetLine S.stdin
        lspLog state $ "> Received Message ---------------------------------"
        lspLog state $ "  line length: " ++ show txContentLength

        -- TODO: handle broken messages, don't just fail with pattern match.
        let Just txLength1  = T.stripPrefix "Content-Length: " txContentLength
        let Just txLength   = T.stripSuffix "\r" txLength1
        let lenChunk        = read (T.unpack txLength)

        lspLog state $ ". (waiting for newline)"
        txEmpty         <- T.hGetLine S.stdin
        lspLog state $ "  line empty: " ++ show txEmpty

        lspLog state $ ". (starting chunk read)"
        txChunk         <- lspReadChunk state lenChunk ""

        lspLog state $ ". (read complete chunk)"
        lspLog state $ "  chunk: " ++ show (T.unpack txChunk) ++ "\n"
        return txChunk


-- | Read a chunk of the given size from stdin.
lspReadChunk :: State -> Int -> T.Text -> IO T.Text
lspReadChunk state n acc
 | T.length acc >= n = return acc
 | otherwise
 = do  lspLog state $ ". (waiting for chunk data)"
       moar   <- T.hGetChunk S.stdin
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


