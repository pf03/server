module App.Server where
    
-- Our Modules
import           Common.Misc
import           Logic.IO.Config
import qualified Logic.IO.Response                as Response
import           T.Transformer

-- Other Modules
import qualified Data.ByteString                  as B
import           Network.Wai                      (Application)
import           Network.Wai.Handler.Warp         as Warp
import Network.Wai.Internal ( getRequestBodyChunk )
import System.Environment ( getEnv )
import Control.Monad ( unless )

run :: IO()
run = do
    mconfig <- setEnvironment
    case mconfig of
        Nothing -> return ()
        Just config -> do
            let port = warpPort . _warp $ config

            putStrLn $ template "Слушаем порт {0}" [show port]
            Warp.run port app

            -- putStrLn $ template "Слушаем порт {0}. Для выхода введите q" [show port]
            -- Warp.runSettings (settings port) app --не работает сервер

app :: Application
app req f = do
    configString <- getEnv "configString"
    --putStrLn "app"
    response <- evalTwithHandler  (Response.get req) Response.errorHandler configString
    emptyBody 0 (getRequestBodyChunk req)
    f response

-- | Если не считывать тело запроса (например при ошибочном запросе пользователя), то вылетает ошибка Error: write ECONNRESET
-- Поэтому используем эту функцию для опустошения тела запроса.
emptyBody :: Int -> IO B.ByteString -> IO ()
emptyBody n str = do
    bs <- str --опустошаем тело запроса
    --print bs
    if bs == mempty
        then do
            putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
        else do
            emptyBody (n+1) str

-- * Ошибка при остановке сервера
-- Network.Socket.accept: failed (Socket operation on non-socket (WSAENOTSOCK))
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
    unless (command == "q") $ stop
