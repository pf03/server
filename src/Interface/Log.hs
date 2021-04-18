{-# LANGUAGE DeriveGeneric #-}
module Interface.Log where

--Our Modules
import qualified Common.Color           as Color
import           Common.Misc

--Other Modules
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (encode, Value (String))
import           Data.Aeson.Types       (FromJSON, ToJSON)
import qualified Data.ByteString        as B
import           GHC.Generics           (Generic)
import           Prelude                hiding (error)
import           System.Console.ANSI
import Data.Char ( toUpper )

-----------------------------Types---------------------------------------------
data LogConfig = LogConfig{
    colorEnable    :: Enable,
    terminalEnable :: Enable,
    fileEnable     :: Enable,
    minLevel       :: Int  --уровень включения логов, в файле удобней указывать Int, а не LogLevel
} deriving (Show, Generic)

instance FromJSON LogConfig
instance ToJSON LogConfig

data LogLevel =  Debug | -- отладочные данные
    Info    | -- информация о работе приложения
    Warn    | -- предупреждения
    Error   | -- некритическая ошибка, которая может в том или ином виде отдаваться пользователю
    Critical  -- критическая ошибка, ведущая к завершению работы приложения
    deriving (Eq, Enum, Ord, Show)

type ColorScheme = Color
type Enable = Bool
-- type FuncName = String
data LogSettings = LogSettings {colorScheme :: ColorScheme, debugMode :: Enable} deriving Show


--объединить Config и Settings в одно
-----------------------------Class---------------------------------------------
class Monad m => MLog m where
  getSettings :: m LogSettings
  setSettings :: ColorScheme -> Enable -> m ()
  getConfig :: m LogConfig
--   setConfig :: LogConfig -> m ()
  message :: LogConfig -> LogSettings -> LogLevel -> String -> m ()

-----------------------------MLog----------------------------------------------
debugOff :: MLog m => m ()
debugOff = do
    LogSettings cs le  <- getSettings
    setSettings cs False

debugOn :: MLog m => m ()
debugOn = do
    LogSettings cs le  <- getSettings
    setSettings cs True

setColorScheme :: MLog m => ColorScheme -> m ()
setColorScheme newcs = do
    LogSettings cs le  <- getSettings
    setSettings newcs le

getConfigSettings :: MLog m => m (LogConfig, LogSettings)
getConfigSettings = do
    config <- getConfig
    settings <- getSettings
    return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = do
    let LogSettings cs e = defaultSettings
    setSettings cs e

logM :: (MLog m, Show a) => m a -> m a
logM m = do
    a <- m
    debugM a
    return a

-- *Эту функцию рекомендуется использовать только при отладке, иначе можно лишиться нужных логов
-- setLevel :: MLog m => LogLevel -> m ()  
-- setLevel newMinLevel = do
--     config <- getConfig
--     setConfig $ config {minLevel = fromEnum newMinLevel}

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

-----------------------------MonadIO-------------------------------------------
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

-- | Реализация класса типов MLog по умолчанию для IO-монады.
-- В чистом коде, например для тестирования, можно заменить эту реализацию на другую, 
-- например основанную на writerT, или на пустую реализацию return ()
-- Info можно показывать в разных цветовых схемах, а для остальных уровней цвет соответствует уровню
messageIO :: MonadIO m => LogConfig -> LogSettings -> LogLevel -> String -> m ()
messageIO (LogConfig ecolor eterminal efile minLevel) (LogSettings colorScheme debugMode) level text = do
    if level < toEnum minLevel && not debugMode then return () else do
        when (ecolor && eterminal ) $ do
            if level == Info then Color.setSchemeT colorScheme
                else Color.setColorT $ getColor level
        when eterminal $ putStrLnT logText
        when efile $ file logText
        when (ecolor && eterminal) Color.resetColorSchemeT
        where
            logText :: String
            logText = map toUpper (show level) <> " " <> text

-----------------------------Simple functions----------------------------------
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
getColor  Debug    = Green
getColor  Info     = Blue -- здесь можно использовать разные цветовые схемы для удобства отображения информации
getColor  Warn     = Magenta
getColor  Error    = Yellow
getColor  Critical = Red
