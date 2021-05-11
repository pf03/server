module App.Server where

-- Our modules
import           Common.Misc
import           Interface.Log            as Log
import           Logic.IO.Config
import qualified Logic.IO.Response        as Response
import           T.Transformer

-- Other modules
import qualified Data.ByteString          as B
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Internal     (getRequestBodyChunk)
import qualified System.Console.ANSI              as Color

run :: IO()
run = do
    mconfig <- exceptToMaybe runConfig
    case mconfig of
        Nothing -> return ()
        Just config -> do
            let port = warpPort . _warp $ config
            let lc = _log config
            Log.infoC lc Color.Green $ template "Start server. Listen to port {0}..." [show port]
            Warp.run port $ app config

app :: Config -> Application
app config req f = do
    response <- evalTwithHandler  (Response.get req) Response.errorHandler config
    emptyBody 0 (getRequestBodyChunk req)
    f response

-- * If you do not read the request body (for example, when there is a wrong user request), then the error occurs "Error: write ECONNRESET"
-- Therefore, we use this function to empty the request body.
emptyBody :: Int -> IO B.ByteString -> IO ()
emptyBody n str = do
    bs <- str
    if bs == mempty
        then do
            putStrLn $ template "Successfully read {0} parts of the request body" [show n]
        else do
            emptyBody (n+1) str



