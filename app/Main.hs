import Network.Wai (responseLBS, Application)
import Network.Wai.Internal ( Request, Response, ResponseReceived )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import qualified Migrations
import Types
import Transformer
import Class
import qualified Log
import System.Environment
import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Response

--с брузера почему то отправляется по два запроса при каждом обновлении страницы.
--через postman один
main = do
    mconfig <- setEnvironment
    case mconfig of 
        Nothing -> return ()
        Just config -> do
            let port = warpPort . _warp $ config
            putStrLn $ "Listening on port " ++ show port
            run port app

app :: Application
app req f = do
    configString <- getEnv "configString"
    putStrLn "app"
    response <- evalT  (Response.get req) Response.rdefault configString
    f $ response
