{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE OverlappingInstances #-}
module Log(
    module Log.Types,
    MonadLog (..),
    sendT,
    receiveT,
    receiveDataT,
    receiveConvertDataT,
    errorT,
    funcT,
    colorTextT,
    textT,
    debugT,
    dataT,
    --prettyT,
    --queryT,
    convertDataT,
    resetSettings,
    error,
    text,
    ldata,
    convertData,
    defaultSettings,
    defaultConfig,
    file,
    clearFile, off, on
    --Pretty(..) 
    ) 
where 

import Log.Types
import qualified Log.Color as Color
import System.Console.ANSI
import Control.Monad
-- import Control.Monad.State.Lazy
-- import App
import Common
import Control.Applicative
import Data.Maybe
import Data.Aeson.Types hiding (Error)
import Data.Aeson hiding (Error)
import Prelude hiding (error)
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Database.PostgreSQL.Simple
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types


class MonadIO m => MonadLog m where
  getSettings :: m LogSettings
  setSettings :: ColorScheme -> Enable -> FuncName -> m ()
  getConfig :: m ConfigLog

off :: MonadLog m => m ()
off = do
    LogSettings cs le fn  <- Log.getSettings
    Log.setSettings cs False fn

on :: MonadLog m => m ()
on = do
    LogSettings cs le fn  <- Log.getSettings
    Log.setSettings cs True fn

--это для возможного переопределения класса show
-- class Show a => Pretty a where
--     show' :: a -> String 
--     --show' = show

-- --такое не работает
-- instance  Show a => Pretty a where
--     show' = show

-- instance  Pretty Query where
--     show' = T.unpack . T.decodeUtf8 . fromQuery

-- logg:: Pretty a => a -> IO()
-- logg a = putStrLn $ pretty a

    --MonadIO m =>
-- showQ :: SQL.Query -> T()
-- showQ = putStrLnT . T.unpack . T.decodeUtf8 . fromQuery


-- instance Show Query where
--     show = T.unpack . T.decodeUtf8 . fromQuery


--подскажите, как переопределить экземпляр класса Show? Допустим я хочу для некоторых типов переопределить экземпляр класса Show,
-- а для остальных типов оставить как есть
-- instance Show Query where
--     show = T.unpack . T.decodeUtf8 . fromQuery

-- log:: Show a => a -> IO()
-- log a = putStrLn $ show a





showQ = T.unpack . T.decodeUtf8 . fromQuery

sendT :: MonadLog m => m ()
sendT = do
    fname <- getfnameT 
    Log.textT Info $ template "Отправляем запрос {0}.................." [fname]

receiveT :: MonadLog m => m ()
receiveT = do 
    fname <- getfnameT 
    Log.textT Info $ template "Получили ответ {0}.................." [fname]

receiveDataT :: (MonadLog m, Show a) => String -> a -> m ()
receiveDataT dataName dataValue = do
    fname <- getfnameT 
    Log.textT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    Log.dataT Data dataValue

receiveConvertDataT :: (MonadLog m, Show a, ToJSON a) => String -> a -> m ()
receiveConvertDataT dataName dataValue = do
    fname <- getfnameT 
    Log.textT Info $ template "Получили {1} в запросе {0}.................." [fname, dataName]
    Log.convertDataT Data dataValue

errorT :: (MonadLog m, Show e) => e -> m ()
errorT error = do
    (config, settings) <- Log.getConfigSettings
    liftIO $ Log.error config settings error

--всего лишь добавляем название функции для удобства отладки
funcT :: MonadLog m => LogLevel -> String -> m ()
funcT level text = do
    fname <- getfnameT 
    textT level $ template "Функция {0}:" [fname]

colorTextT :: MonadLog m => ColorScheme -> LogLevel -> String -> m ()
colorTextT colorScheme level text = do
    Log.setSettings colorScheme True ""
    Log.textT level text

debugT :: MonadLog m => (MonadLog m, Show a) => a -> m ()
debugT = Log.dataT Debug

-- prettyT :: MonadLog m => (MonadLog m, Pretty a) => a -> m ()
-- prettyT dataValue = do 
--     (config, settings) <- Log.getConfigSettings
--     liftIO $ Log.pretty config settings dataValue

-- queryT :: MonadLog m => (MonadLog m) => Query -> m ()
-- queryT dataValue = do 
--     (config, settings) <- Log.getConfigSettings
--     liftIO $ Log.query config settings dataValue

textT :: MonadLog m => LogLevel -> String -> m ()
textT level text = do
    (config, msettings) <- Log.getConfigSettings
    liftIO $ Log.text config msettings level text

dataT :: (MonadLog m, Show a) => LogLevel -> a -> m ()
dataT level dataValue = do
    (config, settings) <- Log.getConfigSettings
    liftIO $ Log.ldata config settings level dataValue

convertDataT :: (MonadLog m, Show a, ToJSON a) => LogLevel -> a -> m ()
convertDataT level dataValue = do
    (config, settings) <- getConfigSettings
    liftIO $ Log.convertData config settings level dataValue

getfnameT :: MonadLog m => m String 
getfnameT = funcName <$> Log.getSettings

getConfigSettings :: MonadLog m => m (ConfigLog, LogSettings) 
getConfigSettings = do
    config <- Log.getConfig 
    settings <- Log.getSettings
    return (config, settings)

_setSettings :: MonadLog m => LogSettings -> m ()
_setSettings (LogSettings s e n) = Log.setSettings s e n

resetSettings :: MonadLog m => m ()
resetSettings = Log._setSettings Log.defaultSettings


--более низкоуровневые функции, если нету доступа к трансформеру, например в runT
--Текст идет с цветовой схемой
error :: (MonadIO m, Show a) => ConfigLog -> LogSettings -> a -> m()
error config settings error = do
    Log.text config settings Error $ template "Получили ошибку в функции {0}!" [funcName settings]
    Log.ldata config settings Error error


text :: MonadIO m => ConfigLog -> LogSettings -> LogLevel -> String -> m ()
text (ConfigLog color terminal file minLevel) (LogSettings colorScheme enable _ ) level text = do
    if not $ level >= toEnum minLevel && enable then return () else do
        when (color && terminal) $ Color.setSchemeT colorScheme
        when terminal $ putStrLnT text
        when file $ Log.file text
        when (color && terminal) Color.resetColorSchemeT 

-- pretty :: (MonadIO m, Pretty a) => ConfigLog -> LogSettings -> a -> m ()
-- pretty cl ls dataValue = _ldata cl ls Debug (show' dataValue)

-- query :: (MonadIO m) => ConfigLog -> LogSettings -> Query -> m ()
-- query cl ls dataValue = _ldata cl ls Debug (showQ dataValue)


--Данные с цветом, зависяцим от LogLevel--logData не зависит от настроек цвета, только logText зависит
ldata :: (MonadIO m,Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> m ()
ldata cl ls level dataValue = _ldata cl ls level (show dataValue)

_ldata :: (MonadIO m) => ConfigLog -> LogSettings -> LogLevel -> String -> m ()
_ldata (ConfigLog color terminal file minLevel) (LogSettings colorScheme enable _ ) level str = do
    if not $ level >= toEnum minLevel && enable then return () else do
        when (color && terminal) $ Color.setColorT $ Log.getColor level
        when terminal $ putStrLnT str
        when file $ Log.file str
        when (color && terminal) Color.resetColorSchemeT 


-------------эти две функции объединить в одну----------------------------------------------------------------
convertData :: (MonadIO m, ToJSON a, Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> m ()
convertData (ConfigLog color terminal file minLevel) (LogSettings colorScheme enable _ ) level dataValue = do
    if not $ level >= toEnum minLevel && enable then return () else do
        when (color && terminal) $ Color.setColorT $ getColor level
        when terminal $ printT dataValue
        --when file $ logFile $ show dataValue
        when file $ Log.file dataValue
        when (color && terminal) Color.resetColorSchemeT 

defaultSettings :: LogSettings
defaultSettings = LogSettings Black True ""

defaultConfig :: ConfigLog
defaultConfig = ConfigLog {colorEnable = False, terminalEnable = True, fileEnable = False, minLevel = 0}

file :: (MonadIO m, ToJSON a) => a -> m()
file str = do 
    liftIO $ B.appendFile "log.txt" $ convert . encode $ str --строгая версия
    liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия

clearFile :: IO()
clearFile = B.writeFile "log.txt" $ convert ("" :: String)

getColor :: LogLevel -> Color
getColor  Data = Green
getColor  Info = Blue
getColor  Error = Red
getColor  Warning = Yellow
getColor  Debug = Magenta 



-- getfname :: LogSettings -> String 
-- -- getfname Nothing = "?"
-- getfname (LogSettings _  _ fn) = fn
