--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module Transformer where 
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import qualified State as S

import qualified System.Console.ANSI as Color

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.Wai (responseLBS, Application)
--наш проект
import qualified Config --40
-- import Error
import Types  --100
-- import Parse
import Error
import qualified Log
import Class
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple
import qualified Parse --50
import Control.Exception
import System.IO.Error (isDoesNotExistError)
import Network.Wai.Handler.Warp (run)
import System.Environment
import qualified Log
import Control.Applicative ((<|>))


--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT

throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  


--все переделать покрсивей
--часть кода скопировать в бот

--эта функция будет использоваться в миграциях и в боте, ее не удалять
--using ExceptT internal monad
--run and show result of transformer
runT :: (ToTransformer m, Show a) => m a -> IO ()
runT m = runE_ $ do 
    let ls = Log.LogSettings Color.Cyan True "runT"
    ---------------------read config---------------------
    (config, _) <- catchE readConfig $ \e -> do
        let dlc = Log.defaultConfig
        Log.text dlc ls Log.Error "Ошибка чтения конфигурации при запуске трансформера: "
        Log.error dlc ls e
        exit
    let lc = _log config
    Log.text lc ls Log.Info "Конфиг успешно считан..."
    ---------------------connect db---------------------
    connection <- catchE (connectDB . _db $ config) $ \e -> do
        Log.text lc ls Log.Error "Ошибка соединения с БД при запуске трансформера: "
        Log.error lc ls e
        exit
    Log.text lc ls Log.Info "БД успешно подключена..."
    let s = getS config connection
    let cl = configLog s
   ---------------------run transformer---------------------
    a <-  catchE (runStateT (toT m) s) $ \e -> do
        Log.text lc ls Log.Error "Ошибка приложения: "
        Log.error lc ls e
        exit
    Log.text lc ls Log.Info "Результат: "
    Log.ldata lc ls Log.Data $ fst a

-- evaluate value of transformer with default value in error case
evalT :: (ToTransformer m) => m a -> a -> String -> IO a
evalT m def configString = runE def $ do    
    let ls = Log.LogSettings Color.Cyan True "runT"
    let dlc = Log.defaultConfig 
    Log.text dlc ls  Log.Info "runT"
    ---------------------read config---------------------
    config <-  toE $ Parse.eDecode  $ read configString
    let lc = _log config
    ---------------------connect db---------------------
    connection <- catchE (connectDB . _db $ config) $ \e -> do
        Log.text lc ls Log.Error "Ошибка соединения с БД при запуске трансформера: "
        Log.error lc ls e
        exit
    Log.text lc ls Log.Info "БД успешно подключена..."
    let s = getS config connection
    let cl = configLog s
    ---------------------run transformer---------------------
    a <-  catchE (runStateT (toT m) s) $ \e -> do
        Log.text lc ls Log.Error "Ошибка приложения: "
        Log.error lc ls e
        exit
    Log.text lc ls Log.Info "Успех"
    --Log.ldata lc ls Log.Data $ fst a
    --возможно нужно в конце отключать бд????
    return $ fst a

--set config as string to the environment, return True if success
setEnvironment :: IO (Maybe Config)
setEnvironment = runE Nothing $ do
    let ls = Log.LogSettings Color.Cyan True "runT"
    ---------------------read config---------------------
    (config, configString) <- catchE readConfig $ \e -> do
        let dlc = Log.defaultConfig
        Log.text dlc ls Log.Error "Ошибка чтения конфигурации при запуске трансформера: "
        Log.error dlc ls e
        exit
    let lc = _log config
    --передать весь конфиг как строку 
    Log.text lc ls Log.Info "Конфиг успешно считан..."
    lift $ setEnv "configString" configString
    return $ Just config


--exit from ExceptT transformer with error
exit :: ExceptT E IO a
exit = ExceptT $ do
    return $ Left $ SomeError ""

--run ExceptT transformer without error handling with default value
runE :: a -> ExceptT E IO a -> IO a
runE a m = do
    putStrLn "runE_"
    --return a
    eb <- runExceptT m  
    case eb of 
        Left e -> return a
        Right b -> return b

--value doesn't matter
runE_ :: ExceptT E IO () -> IO()
runE_ m = runExceptT m >> return ()

--read config as both object and string
readConfig :: ExceptT E IO (Config, String)
readConfig = do
    bs <- ExceptT $ toEE (L.readFile pathConfig) `catch` handler
    fileConfig <- toE $ Parse.eDecode bs
    --print fileConfig
    return (fileConfig, show bs) where
        handler :: IOException -> IO (EE L.ByteString )
        handler e
            | isDoesNotExistError e = return $ Left $ ConfigError "Файл конфигурации не найден!"
            | otherwise = return $ Left  $ ConfigError "Ошибка чтения файла конфигурации"

connectDB :: ConnectInfo -> ExceptT E IO Connection
connectDB connectInfo = do
    conn <- ExceptT $ toEE (connect connectInfo) `catch` handler 
    return conn where 
        handler :: SqlError -> IO (EE Connection )
        handler e = return . Left  . DBError $ "Ошибка соединения с базой данных!"


pathConfig :: FilePath
pathConfig = "config.json"

-------------------State <-> Config--------------------------------------
getS :: Config -> Connection -> S
getS Config {_warp = configWarp, _db = _, _log = configLog} connection = S {
    configWarp = configWarp,
    connectionDB = connection,
    configLog = configLog, 
    logSettings = Log.defaultSettings 
}

---------------------------------------MonadLog Test-------------------------------------------------------
testLog :: IO()
testLog = runT $ do
    Log.dataT Log.Debug $ "Debug data value " ++ show [1..10]  :: T()
    Log.dataT Log.Info $ "Info data value " ++ show [1..10] 
    Log.dataT Log.Error $ "Error data value " ++ show [1..10] 
    Log.dataT Log.Data $ "Data data value " ++ show [1..10] 
    Log.dataT Log.Warning  $ "Warning data value " ++ show [1..10] 
    Log.colorTextT Color.Blue Log.Debug $"Blue color scheme " ++ klichko
    Log.colorTextT Color.Cyan Log.Debug $ "Cyan color scheme " ++ klichko
    Log.colorTextT Color.Green Log.Debug $ "Green color scheme " ++ klichko
    Log.colorTextT Color.Yellow Log.Debug $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"
