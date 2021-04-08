import Network.Wai (responseLBS, Application)
import Network.Wai.Internal --( Request, Response, ResponseReceived )
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
import  qualified Data.ByteString as B
import Common

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
    response <- evalTwithHandler  (Response.get req :: T Response) Response.errorHandler configString
    emptyBody 0 (getRequestBodyChunk req)
    f $ response


--Если не считывать тело запроса (например при ошибочном запросе пользователя), то вылетает ошибка Error: write ECONNRESET
--Поэтому используем этот костыль для опустошения тела запроса. Правильно решить этот вопрос у меня не хватает знаний.
emptyBody :: Int -> IO B.ByteString -> IO ()
emptyBody n str = do
    bs <- str --опустошаем тело запроса
    --print bs
    if bs == mempty 
        then do
            putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
        else do
            emptyBody (n+1) str