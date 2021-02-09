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

import System.Console.ANSI

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


--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT

throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  



--запуск основного трансформера и всех монад попроще
--трансформер есть кусок программы, который можно запускать независимо от основной программы,
--поэтому в runT уже имеется и считывание config, и подключение к БД, и т. д.
--эту функцию переписать полаконичнее
-- runT :: (ToTransformer m, Show a) => m a -> IO()
-- runT m = do 
--     let ls = Log.LogSettings Cyan True "runT"
--     eConfig <- runExceptT readConfig
--     case eConfig of
--         Left e -> do
--             let dlc = Log.defaultConfig
--             Log.text dlc ls Log.Error "Ошибка запуска трансформера: "
--             Log.error dlc ls e
--         Right config -> do
--             let lc = _log config
--             Log.text lc ls Log.Info "Конфиг успешно считан..."
--             eConnection <- runExceptT . connectDB . _db $ config 
--             case eConnection of
--                 Left e -> do
--                     Log.text lc ls Log.Error "Ошибка запуска трансформера: "
--                     Log.error lc ls e
--                 Right connection -> do
--                     Log.text lc ls Log.Info "БД успешно подключена..."
--                     let s = getS config connection
--                     let cl = configLog s
--                     ea <- runExceptT $ runStateT (toT m) s
--                     case ea  of
--                         Left e -> do 
--                             Log.text lc ls Log.Error "Ошибка приложения: "
--                             Log.error lc ls e
--                         Right a -> do 
--                             Log.text lc ls Log.Info "Результат: "
--                             Log.ldata lc ls Log.Data $ fst a

--using ExceptT internal monad
runT :: (ToTransformer m, Show a) => m a -> IO ()
runT m = runE $ do 
    let ls = Log.LogSettings Cyan True "runT"
    ---------------------read config---------------------
    config <- catchE readConfig $ \e -> do
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
    ---------------------start warp-----------------------

    a <-  catchE (runStateT (toT m) s) $ \e -> do
        Log.text lc ls Log.Error "Ошибка приложения: "
        Log.error lc ls e
        exit
    Log.text lc ls Log.Info "Результат: "
    Log.ldata lc ls Log.Data $ fst a

runApp :: ApplicationT -> T ()
runApp appT = do
    let port = 80
    toT $ putStrLn $ "Listening on port " ++ show port
    run port appT
    undefined

runAppT_ :: ApplicationT -> T Application
runAppT_ funcT = \req fT -> do 
    f < -  fT
    funcT req f
    



--exit from ExceptT transformer
exit :: ExceptT E IO a
exit = ExceptT $ do
    return $ Left $ SomeError ""

runE :: ExceptT E IO () -> IO()
runE m = runExceptT m >> return ()

-- handler :: (ToTransformer m, Show a) => e -> ExceptT E m a
-- handler = do
--     let dlc = Log.defaultConfig
--     Log.text dlc ls Log.Error "Ошибка запуска трансформера: "
--     Log.error dlc ls e

pathConfig :: FilePath
pathConfig = "config.json"

readConfig :: ExceptT E IO Config
readConfig = do
    bs <- ExceptT $ toEE (L.readFile pathConfig) `catch` handler
    fileConfig <- toE $ Parse.eDecode bs
    --print fileConfig
    return fileConfig where
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


-- readS :: ExceptT E IO S
-- readS = do
--     config <- readConfig
--     conn <- connectDB $ _db config 
--     let s = toS config conn
--     --print config
--     return s

-------------------State <-> Config--------------------------------------
getS :: Config -> Connection -> S
getS Config {_warp = configWarp, _db = _, _log = configLog} connection = S {
    configWarp = configWarp,
    connectionDB = connection,
    configLog = configLog, 
    logSettings = Log.defaultSettings 
}

-- fromS :: Config -> S -> Config
-- fromS configFile config = undefined

---------------------------------------MonadLog Test-------------------------------------------------------
testLog :: IO()
testLog = runT $ do
    Log.dataT Log.Debug $ "Debug data value " ++ show [1..10]  :: T()
    Log.dataT Log.Info $ "Info data value " ++ show [1..10] 
    Log.dataT Log.Error $ "Error data value " ++ show [1..10] 
    Log.dataT Log.Data $ "Data data value " ++ show [1..10] 
    Log.dataT Log.Warning  $ "Warning data value " ++ show [1..10] 
    Log.colorTextT Blue Log.Debug $"Blue color scheme " ++ klichko
    Log.colorTextT Cyan Log.Debug $ "Cyan color scheme " ++ klichko
    Log.colorTextT Green Log.Debug $ "Green color scheme " ++ klichko
    Log.colorTextT Yellow Log.Debug $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"
