--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module Transformer  where
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import qualified State as S

import qualified System.Console.ANSI as Color


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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
import Common
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson


--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT

throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  


------------------------------------------IO---------------------------------------------------------------------------

--часть кода скопировать в бот
--эта функция будет использоваться в миграциях и в боте, ее не удалять
--using ExceptT internal monad
--run and show result of transformer
showT :: (ToTransformer m, Show a) => m a -> IO ()
showT m = runE_ $ do
    (config, _) <- _runConfig
    connection <- _runConnection config
    value <-_getValue config connection m
    _showValue config value

--run transformer without showing
runT :: ToTransformer m => m a -> IO ()
runT m = runE_ $ do
    (config, _) <- _runConfig
    connection <- _runConnection config
    _getValue config connection m
    return ()

-- evaluate value of transformer with default value in error case
-- configString red from Environment
evalT :: (ToTransformer m) => m a -> a -> String -> IO a
evalT m def configString = runE def $ do    
    config <-  toE $ Parse.eDecode  $ read configString
    connection <- _runConnection config
    _getValue config connection m

-- evaluate value of transformer with default value in error case
--configString red from Environment
evalTwithHandler :: (ToTransformer m) => m a -> (E -> a) -> String -> IO a
evalTwithHandler m handler configString = runEwithHandler handler $ do    
    config <-  toE $ Parse.eDecode  $ read configString
    connection <- _runConnection config
    _getValue config connection m

--set config as string to the environment, return True if success
setEnvironment :: IO (Maybe Config)
setEnvironment = runE Nothing $ do
    (config, configString) <- _runConfig
    lift $ setEnv "configString" configString
    return $ Just config

--internal functions
--тавтология с readConfig
_runConfig :: ExceptT E IO (Config, String)
_runConfig = do
    let ls = Log.LogSettings Color.Cyan True "runT"
    ---------------------read config---------------------
    (config, configString) <- catchE readConfig $ \e -> do
        let dlc = Log.defaultConfig
        Log.text dlc ls Log.Error "Ошибка чтения конфигурации при запуске трансформера: "
        Log.error dlc ls e
        throwE e
    let lc = _log config
    Log.text lc ls Log.Info "Конфиг успешно считан..."
    return (config, configString)

_runConnection :: Config -> ExceptT E IO Connection
_runConnection config = do
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    connection <- catchE (connectDB . _db $ config) $ \e -> do
        Log.text lc ls Log.Error "Ошибка соединения с БД при запуске трансформера: "
        Log.error lc ls e
        throwE e
    Log.text lc ls Log.Info "БД успешно подключена..."
    let s = getS config connection
    let cl = configLog s
    return connection

_getValue :: (ToTransformer m) => Config -> Connection -> m a -> ExceptT E IO a
_getValue config connection m = do
    let s = getS config connection
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    a <-  catchE (runStateT (toT m) s) $ \e -> do
        Log.text lc ls Log.Error "Ошибка приложения: "
        Log.error lc ls e
        throwE e
    return $ fst a 

_showValue :: (Show a) => Config -> a -> ExceptT E IO ()
_showValue config value = do 
    let s = getS config
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    Log.text lc ls Log.Info "Результат: "
    Log.ldata lc ls Log.Data value
    return ()



--------------------------------------------ExceptT E IO a--------------------------------------------
--exit from ExceptT transformer with error
-- exit :: ExceptT E IO a
-- exit = ExceptT $ do
--     return $ Left $ SomeError ""

--run ExceptT transformer without error handling with default value
runE :: a -> ExceptT E IO a -> IO a
runE a m = do
    putStrLn "runE_"
    --return a
    eb <- runExceptT m  
    case eb of 
        Left e -> return a
        Right b -> return b
        
--run ExceptT transformer with error handling
runEwithHandler :: (E -> a) -> ExceptT E IO a -> IO a
runEwithHandler handler m = do
    putStrLn "runE_"
    --return a
    eb <- runExceptT m  
    case eb of 
        Left e -> return $ handler e
        Right b -> return b

fromE ::  ExceptT E IO a -> IO a
fromE m = do
    putStrLn "fromE"
    --return a
    eb <- runExceptT m  
    case eb of 
        Left e -> error $ "error in fromE with exception: " ++ show e
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

pathConfig :: FilePath
pathConfig = "config.json"

connectDB :: ConnectInfo -> ExceptT E IO Connection
connectDB connectInfo = do
    conn <- ExceptT $ toEE (connect connectInfo) `catch` handler 
    return conn where 
        handler :: SqlError -> IO (EE Connection )
        handler e = return . Left  . DBError $ "Ошибка соединения с базой данных!"




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

--переместить в какой-то другой модуль
-----------------работа с файлами-----------------------------------------------------------------------------------
readFile :: String -> ExceptT E IO B.ByteString
readFile path = do
    bs <- ExceptT $ toEE (BC.readFile path) `catch` handler
    --print fileConfig
    return bs where
        handler :: IOException -> IO (EE B.ByteString )
        handler e
            | isDoesNotExistError e = return $ Left $ IOError $ template  "Файл \"{0}\" не найден!" [path]
            | otherwise = return $ Left  $ IOError  $ template "Ошибка чтения файла \"{0}\"" [path]

writeResponse :: (ToJSON a) => a -> T()
writeResponse json = do
    Log.colorTextT Color.Yellow Log.Warning "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json --строгая версия
    --liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия