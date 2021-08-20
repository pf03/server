module App.Server where

import Common.Functions (Template (template))
import qualified Data.ByteString as B
import qualified Interface.MLog.Exports as Log
import Logic.IO.Config (Config, ConfigWarp (warpPort))
import qualified Logic.IO.Config as Config
import qualified Logic.IO.Response.Functions as Response
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Internal (getRequestBodyChunk)
import qualified Transformer.Exports as Transformer

run :: IO ()
run = do
  mConfig <- Transformer.exceptToMaybe Transformer.runConfig
  case mConfig of
    Nothing -> return ()
    Just config -> do
      let port = warpPort . Config.warp $ config
      let logConfig = Config.log config
      Log.writeInfoColor logConfig Log.GreenScheme $ template "Start server. Listen to port {0}..." [show port]
      Warp.run port $ app config

app :: Config -> Application
app config request f = do
  response <- Transformer.evalTWithHandler (Response.get request) Response.errorHandler config
  emptyBody 0 (getRequestBodyChunk request)
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
      emptyBody (n + 1) str
