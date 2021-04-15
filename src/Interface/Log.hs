{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Interface.Log
(
    module Interface.Log,
    -- MLog (..),
    -- sendT,
    -- receiveT,
    -- receiveDataT,
    -- receiveConvertDataT,
    -- errorT,
    -- funcT,
    -- colorTextT,
    -- textT,
    -- debugT,
    -- dataT,
    -- --prettyT,
    -- --queryT,
    -- convertDataT,
    -- resetSettings,
    -- error,
    -- text,
    -- ldata,
    -- convertData,
    -- defaultSettings,
    -- defaultConfig,
    -- file,
    -- clearFile, off, on, logM
    --Pretty(..) 
    ) 
where 
import GHC.Generics hiding (S)
import qualified Common.Color as Color
import System.Console.ANSI
import Control.Monad
-- import Control.Monad.State.Lazy
-- import App
import Common.Misc
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

-----------------------------Types---------------------------------------------
data LogConfig = LogConfig{
    colorEnable :: Enable,
    terminalEnable :: Enable,
    fileEnable :: Enable,
    minLevel :: Int  --уровень включения логов, в файле удобней указывать Int, а не LogLevel
} deriving (Show, Generic)

instance FromJSON LogConfig
instance ToJSON LogConfig

data LogLevel =  Debug | -- отладочные данные
    Info    | --информация о работе приложения
    Warn    | --предупреждения
    Error   | --некритическая ошибка, которая может в том или ином виде отдаваться пользователю
    Critical  --критическая ошибка, ведущая к завершению работы приложения
    deriving (Eq, Enum, Ord, Show)
type ColorScheme = Color
type Enable = Bool
-- type FuncName = String
data LogSettings = LogSettings {colorScheme :: ColorScheme, logEnable :: Enable} deriving Show

-----------------------------Class---------------------------------------------
class Monad m => MLog m where
  getSettings :: m LogSettings
  setSettings :: ColorScheme -> Enable -> m ()
  getConfig :: m LogConfig
  message :: LogConfig -> LogSettings -> LogLevel -> String -> m ()

off :: MLog m => m ()
off = do
    LogSettings cs le  <- getSettings
    setSettings cs False

on :: MLog m => m ()
on = do
    LogSettings cs le  <- getSettings
    setSettings cs True

setColorScheme :: MLog m => ColorScheme -> m ()
setColorScheme newcs = do
    LogSettings cs le  <- getSettings
    setSettings newcs le

logM :: (MLog m, Show a) => m a -> m a 
logM m = do
    a <- m
    debugM a
    return a 

-- showQ = T.unpack . T.decodeUtf8 . fromQuery

--message - общее
-- debug для Show a, остальное для текста

-- messageM :: MLog m => LogLevel -> String -> m ()
-- messageM = do
--     (config, settings) <- getConfigSettings
--     undefined

-- | Для отладочной информации сделано исключение - она может быть любого типа Show a, 
-- а не только строкового
debugM :: (MLog m, Show a) => a -> m ()
debugM a = messageM Debug (show a)

infoM :: MLog m => String -> m ()
infoM = messageM Info

infoCM :: MLog m => ColorScheme -> String -> m ()
infoCM cs s = do
    setColorScheme cs
    infoM s

warnM :: MLog m => String -> m ()
warnM = messageM Warn

errorM :: MLog m => String -> m ()
errorM = messageM Error

criticalM :: MLog m => String -> m ()
criticalM = messageM Critical

messageM :: MLog m => LogLevel -> String -> m()
messageM level s = do
    (config, settings) <- getConfigSettings
    message config settings level s

debug :: (MonadIO m, Show a) => LogConfig -> LogSettings -> a -> m ()
debug lc ls a = messageIO lc ls Debug (show a)

info :: MonadIO m => LogConfig -> LogSettings -> String -> m ()
info lc ls = messageIO lc ls Info

warn :: MonadIO m => LogConfig -> LogSettings -> String -> m ()
warn lc ls = messageIO lc ls Warn

error :: MonadIO m => LogConfig -> LogSettings -> String -> m ()
error lc ls = messageIO lc ls Error

critical :: MonadIO m => LogConfig -> LogSettings -> String -> m ()
critical lc ls = messageIO lc ls Critical


-- debugM :: (MLog m, Show a) => a -> m ()
-- debugM = dataT Debug

-- text :: MLog m => LogLevel -> String -> m ()
-- textT level t = do
--     (config, msettings) <- getConfigSettings
--     liftIO $ text config msettings level t

-- dataT :: (MLog m, Show a) => LogLevel -> a -> m ()
-- dataT level dataValue = do
--     (config, settings) <- getConfigSettings
--     liftIO $ ldata config settings level dataValue

-- convertDataT :: (MLog m, Show a, ToJSON a) => LogLevel -> a -> m ()
-- convertDataT level dataValue = do
--     (config, settings) <- getConfigSettings
--     liftIO $ convertData config settings level dataValue


getConfigSettings :: MLog m => m (LogConfig, LogSettings) 
getConfigSettings = do
    config <- getConfig 
    settings <- getSettings
    return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = do
    let LogSettings cs e = defaultSettings
    setSettings cs e


--Текст идет с цветовой схемой
-- error :: (MonadIO m, Show a) => ConfigLog -> LogSettings -> a -> m()
-- error config settings error = do
--     ldata config settings Error error



-- Реализация класса типов MLog по умолчанию для IO-монады. 
-- В чистом коде, например для тестирования, можно заменить эту реализацию на другую, например основанную на writerT, 
-- или на пустую реализацию return ()
-- Info можно показывать в разных цветовых схемах, а для остальных уровней цвет соответствует уровню
messageIO :: MonadIO m => LogConfig -> LogSettings -> LogLevel -> String -> m ()
messageIO (LogConfig ecolor eterminal efile minLevel) (LogSettings colorScheme enable) level text = do
    if not $ level >= toEnum minLevel && enable then return () else do
        when (ecolor && eterminal ) $ do
            if level == Info then Color.setSchemeT colorScheme
                else Color.setColorT $ getColor level
        when eterminal $ putStrLnT text
        when efile $ file text
        when (ecolor && eterminal) Color.resetColorSchemeT


--Данные с цветом, зависяцим от LogLevel--logData не зависит от настроек цвета, только logText зависит
-- ldata :: (MonadIO m,Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> m ()
-- ldata cl ls level dataValue = _ldata cl ls level (show dataValue)

-- _ldata :: (MonadIO m) => ConfigLog -> LogSettings -> LogLevel -> String -> m ()
-- _ldata (ConfigLog color terminal file minLevel) (LogSettings colorScheme enable) level str = do
--     if not $ level >= toEnum minLevel && enable then return () else do
--         when (color && terminal) $ Color.setColorT $ getColor level
--         when terminal $ putStrLnT str
--         when file $ file str
--         when (color && terminal) Color.resetColorSchemeT 


-------------эти две функции объединить в одну----------------------------------------------------------------
-- convertData :: (MonadIO m, ToJSON a, Show a) => ConfigLog -> LogSettings -> LogLevel -> a -> m ()
-- convertData (ConfigLog color terminal file minLevel) (LogSettings colorScheme enable) level dataValue = do
--     if not $ level >= toEnum minLevel && enable then return () else do
--         when (color && terminal) $ Color.setColorT $ getColor level
--         when terminal $ printT dataValue
--         --when file $ logFile $ show dataValue
--         when file $ file dataValue
--         when (color && terminal) Color.resetColorSchemeT 

defaultSettings :: LogSettings
defaultSettings = LogSettings Black True

defaultConfig :: LogConfig
defaultConfig = LogConfig {colorEnable = False, terminalEnable = True, fileEnable = False, minLevel = 0}

file :: (MonadIO m, ToJSON a) => a -> m()
file str = do 
    liftIO $ B.appendFile "log.txt" $ convert . encode $ str 
    liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String) 

clearFile :: IO()
clearFile = B.writeFile "log.txt" $ convert ("" :: String)

getColor :: LogLevel -> Color
getColor  Debug = Green 
getColor  Info = Blue --здесь можно использовать разные цветовые схемы для удобства отображения информации
getColor  Warn = Magenta
getColor  Error = Yellow
getColor  Critical = Red 
