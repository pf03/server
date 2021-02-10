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

-- main = runT app
    -- let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
    -- conn <- connect connectDBInfo
    -- -- let port = 80
    -- -- putStrLn $ "Listening on port " ++ show port
    -- -- run port app
    -- query_ conn [sql| SELECT id, first_name FROM employee |] :: IO [(Int, String)]
main = do
    eConfig_ <- runExceptT readConfig_
    case eConfig_ of 
        Left e -> return()
        Right (config, configString) -> do
            setEnvirnoment 
            let port = warpPort . _warp $ config
            putStrLn $ "Listening on port " ++ show port
            run port app

app :: Application
app req f = do
    -- test <- getEnv "test"
    -- putStrLn test
    --env <- getEnvironment
    --print env
    configString <- getEnv "configString"
    putStrLn "app"
    response <- runT2  (Response.get req) Response.rdefault configString
    f $ response


-- main :: IO()
-- main = runT mainT
--     runAppT app 

-- app :: Application
-- app req f = do
--     --undefined
--     print req
--     received <- f $ response req
--     return received

-- responseT :: Request -> T Response
-- responseT req = do 
--     return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

-- mainT :: T()
-- mainT = do
--     s <- get 
--     sVar <- toT $ newMVar s
--     let port = 80
--     Log.textT Log.Info $ "Listening on port " ++ show port
--     toT $ run port app


-- Немного не пойму логику работы сервера. Получается, что функция app запускается при каждом запросе. А как ей передать настройки базы данных и пр.? 
-- При каждом запросе считывать с файла не очень эффективно. В доке указано, что для передачи порта можно использовать некоторые environment variables, я так понял System.Environment
-- Просто первый раз об этом слышу. Это типа глобальных переменных? 

-- main = do
--     let port = 80
--     putStrLn $ "Listening on port " ++ show port
--     run port app

-- app :: Application -- (== Request -> (Response -> IO ResponseReceived) -> IO ResponseReceivedSource)
-- app req f = do
--     putStrLn "app"
--     f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"