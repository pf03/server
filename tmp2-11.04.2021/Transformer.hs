--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Transformer  where
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import State as S

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
--import Error
import ToTransformer
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple
import Control.Exception
import System.IO.Error (isDoesNotExistError)
import Network.Wai.Handler.Warp (run)
import System.Environment
import qualified Log
import Control.Applicative ((<|>))
import Common
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import DB
import qualified Error
import Error -- (MError, MIOError,throw, catch)
import Cache
import Control.Exception as Exception

-- | Данный модуль реализует класс типов MT (и остальные классы) с помощью трансформера T

-----------------------------Instances-----------------------------------------

instance Log.MLog T where 
  getSettings = S.getLogSettings
  setSettings = S.setLogSettings
  getConfig = S.getLogConfig

instance MError T where
    -- throw = throwT
    -- catch = catchT where

    throw :: E -> T a
    throw e  = toT (throwE e::Except E a)

    catch :: T a -> (E -> T a) -> T a
    catch ta f  = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s
    
instance MIOError T

instance MCache T where
    getCache = gets cache
    setCache cache = modify (\st -> st {cache = cache})

instance MDB T where
    getConnection = gets connectionDB

instance MT T

-----------------------------EXTERNAL------------------------------------------
-- | Run and show result of transformer
showT :: (ToTransformer m, Show a) => m a -> IO ()
showT m = runE_ $ do
    (config, _) <- _runConfig
    connection <- _runConnection config
    value <-_getValue config connection m
    _showValue config value

-- | Run transformer without showing
runT :: ToTransformer m => m a -> IO ()
runT m = runE_ $ do
    (config, _) <- _runConfig
    connection <- _runConnection config
    _getValue config connection m
    return ()

-- | Evaluate value of transformer with default value in error case
-- * ConfigString red from Environment
evalT :: (ToTransformer m) => m a -> a -> String -> IO a
evalT m def configString = runE def $ do
    config <- eDecode  $ read configString
    connection <- _runConnection config
    _getValue config connection m

-- | Evaluate value of transformer with default value in error case
-- * ConfigString red from Environment
evalTwithHandler :: (ToTransformer m) => m a -> (E -> a) -> String -> IO a
evalTwithHandler m handler configString = runEwithHandler handler $ do
    config <- eDecode  $ read configString
    connection <- _runConnection config
    _getValue config connection m

-- | Set config as string to the environment, return True if success
setEnvironment :: IO (Maybe Config)
setEnvironment = runE Nothing $ do
    (config, configString) <- _runConfig
    lift $ setEnv "configString" configString
    return $ Just config

-----------------------------INTERNAL------------------------------------------
--тавтология с readConfig
_runConfig :: (MIOError m) => m (Config, String)
_runConfig = do
    let ls = Log.LogSettings Color.Cyan True "runT"
    ---------------------read config---------------------
    (config, configString) <- Error.catch readConfig $ \e -> do
        let dlc = Log.defaultConfig
        Log.text dlc ls Log.Error "Ошибка чтения конфигурации при запуске трансформера: "
        Log.error dlc ls e
        Error.throw e
    let lc = _log config
    Log.text lc ls Log.Info "Конфиг успешно считан..."
    return (config, configString)

_runConnection :: (MIOError m) => Config -> m Connection
_runConnection config = do
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    connection <- Error.catch (DB.connectDB . _db $ config) $ \e -> do
        Log.text lc ls Log.Error "Ошибка соединения с БД при запуске трансформера: "
        Log.error lc ls e
        Error.throw e
    Log.text lc ls Log.Info "БД успешно подключена..."
    let s = getS config connection
    let cl = configLog s
    return connection

_getValue :: (ToTransformer m) => Config -> Connection -> m a -> ExceptT E IO a
_getValue config connection m = do
    let s = getS config connection
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    a <-  Error.catch (runStateT (toT m) s) $ \e -> do
        Log.text lc ls Log.Error "Ошибка приложения: "
        Log.error lc ls e
        Error.throw e
    return $ fst a

_showValue :: (MonadIO m, Show a) => Config -> a -> m ()
_showValue config value = do
    let s = getS config
    let ls = Log.LogSettings Color.Cyan True "runT"
    let lc = _log config
    Log.text lc ls Log.Info "Результат: "
    Log.ldata lc ls Log.Data value
    return ()

-----------------------------ExceptT E IO a------------------------------------
-- | Run ExceptT transformer without error handling with default value
runE :: a -> ExceptT E IO a -> IO a
runE a m = do
    eb <- runExceptT m
    case eb of
        Left e -> return a
        Right b -> return b

-- | Run ExceptT transformer with error handling
runEwithHandler :: (E -> a) -> ExceptT E IO a -> IO a
runEwithHandler handler m = do
    eb <- runExceptT m
    case eb of
        Left e -> return $ handler e
        Right b -> return b

-- | Value doesn't matter
runE_ :: ExceptT E IO () -> IO()
runE_ m = void (runExceptT m)

-----------------------------CONFIG--------------------------------------------
getS :: Config -> Connection -> S
getS Config {_warp = configWarp, _db = _, _log = configLog} connection = S {
    configWarp = configWarp,
    connectionDB = connection,
    configLog = configLog,
    logSettings = Log.defaultSettings,
    cache = Cache{changed = mempty, auth = AuthNo, params = mempty}
}

-- | Read config as both object and string
readConfig :: MIOError m => m (Config, String)
readConfig = do
    bs <- L.readFile pathConfig `Error.catchEIO` handler
    fileConfig <- eDecode bs
    return (fileConfig, show bs) where
        handler :: IOException -> E
        handler e
            | isDoesNotExistError e = ConfigError "Файл конфигурации не найден!"
            | otherwise = ConfigError "Ошибка чтения файла конфигурации"

pathConfig :: FilePath
pathConfig = "config.json"

-----------------------------DECODE--------------------------------------------
eDecode :: (MError m, FromJSON a) => LC.ByteString -> m a
eDecode bs = catchEither (eitherDecode bs) ParseError

-----------------------------LOG TEST------------------------------------------
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
