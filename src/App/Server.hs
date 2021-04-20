module App.Server where


-- Our Modules
import           Common.Misc
import           Logic.IO.Config
import qualified Logic.IO.Response                as Response
import           T.State
import           T.Transformer

-- Other Modules
import qualified Data.ByteString                  as B
import           Database.PostgreSQL.Simple.SqlQQ
import           Network.HTTP.Types               (status200)
import           Network.HTTP.Types.Header        (hContentType)
import           Network.Wai                      (Application, responseLBS)
import           Network.Wai.Handler.Warp         as Warp
import           Network.Wai.Internal
import           System.Environment
import           System.Exit
import Control.Concurrent
import Control.Monad

--с брузера почему то отправляется по два запроса при каждом обновлении страницы.
--через postman один
run :: IO()
run = do
    mconfig <- setEnvironment
    case mconfig of
        Nothing -> return ()
        Just config -> do
            let port = warpPort . _warp $ config
            putStrLn $ template "Слушаем порт {0}. Для выхода введите q" [show port]
            Warp.runSettings (settings port) app

app :: Application
app req f = do
    configString <- getEnv "configString"
    putStrLn "app"
    response <- evalTwithHandler  (Response.get req) Response.errorHandler configString
    emptyBody 0 (getRequestBodyChunk req)
    f response


--Если не считывать тело запроса (например при ошибочном запросе пользователя), то вылетает ошибка Error: write ECONNRESET
--Поэтому используем эту функцию для опустошения тела запроса.
emptyBody :: Int -> IO B.ByteString -> IO ()
emptyBody n str = do
    bs <- str --опустошаем тело запроса
    --print bs
    if bs == mempty
        then do
            putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
        else do
            emptyBody (n+1) str


--ошибка при остановке сервера
settings :: Port -> Settings
settings port = setPort port . 
    setInstallShutdownHandler shutdownHandler . 
    setGracefulShutdownTimeout (Just 1) $ 
    defaultSettings
  where
    shutdownHandler closeSocket = do
        stop
        closeSocket

stop :: IO ()
stop = do
    command <- getLine
    putStrLn command
    if command == "q" 
        then return () --exitSuccess 
        else stop
