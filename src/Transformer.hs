--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- import Error
import Types  --100
-- import Parse
import Error
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

catchT :: T a -> (E -> T a) -> T a
catchT ta f  = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

--это уже есть
class Monad m => MError m where
    throwM :: E -> m a
    catchM :: m a -> (E -> m a) -> m a

-- instance MonadError E T where
--     throwError = throwT
--     catchError = catchT

instance MError T where
    throwM = throwT
    catchM = catchT



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
runE_ m = void (runExceptT m)

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
connectDB connectInfo = ExceptT $ toEE (connect connectInfo) `catch` handler where
    handler :: SqlError -> IO (EE Connection )
    handler e = return . Left  . DBError $ "Ошибка соединения с базой данных!"




-------------------State <-> Config--------------------------------------
getS :: Config -> Connection -> S
getS Config {_warp = configWarp, _db = _, _log = configLog} connection = S {
    configWarp = configWarp,
    connectionDB = connection,
    configLog = configLog,
    logSettings = Log.defaultSettings,
    changed = mempty,
    auth = AuthNo,
    params = mempty
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
readFile path = ExceptT $ toEE (BC.readFile path) `catch` handler where
    handler :: IOException -> IO (EE B.ByteString )
    handler e
        | isDoesNotExistError e = return $ Left $ IOError $ template  "Файл \"{0}\" не найден!" [path]
        | otherwise = return $ Left  $ IOError  $ template "Ошибка чтения файла \"{0}\"" [path]

writeResponse :: (ToJSON a) => a -> T()
writeResponse json = do
    Log.colorTextT Color.Yellow Log.Warning "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json --строгая версия
    --liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия

writeResponseJSON :: LC.ByteString -> T()
writeResponseJSON json = do
    Log.colorTextT Color.Yellow Log.Warning "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert json --строгая версия
    --liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия


-----------------------------------------------------------------------------------------------------------------------
--для работы с телом запроса, поделенным на чанки
stream :: (Eq a, Monoid a) => IO a -> (a -> IO()) -> IO()
stream source receiver = helper 0 source receiver where
  helper n source receiver = do
      a <- source
      if a == mempty
        then putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
        else do
            receiver a
            helper (n+1) source receiver

--для работы с телом запроса, поделенным на чанки - должно быть не более одного чанка
streamOne :: (Eq a, Monoid a) => IO a -> ExceptT E IO a
streamOne source = helper 0 source where
    helper n source = do
        liftIO $ putStrLn "streamOne"
        a <- liftIO source
        if a == mempty
            then do
                liftIO $ putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
                return a
            else if n>=1
                then throwE $ RequestError "Слишком длинное тело запроса. Тело запроса должно состоять не более, чем из одного чанка"
                else (a <>) <$> helper (n + 1) source

--для работы с телом запроса, поделенным на чанки - должно быть пустым
streamEmpty :: (Eq a, Monoid a) => IO a -> ExceptT E IO ()
streamEmpty source = do
    a <- liftIO source
    if a == mempty
        then return ()
        else throwE $ RequestError "Тело запроса должно быть пустым"


-- ltest :: IO ()
-- ltest = runT $ do
--     Log.colorTextT Color.Yellow Log.Debug  "Yellow." :: T()
--     Log.colorTextT Color.Blue Log.Debug  "Blue."