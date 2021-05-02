{-# LANGUAGE DeriveGeneric #-}
module Interface.Log where

--Our modules
import qualified Common.Color           as Color
import           Common.Misc

--Other modules
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
    -- | Minimal log level as an integer
    minLevel       :: Int 
} deriving (Show, Generic)

instance FromJSON LogConfig
instance ToJSON LogConfig

data LogLevel =  
    Debug   | -- Debug data
    Info    | -- Information about app work
    Warn    | -- Warnings
    Error   | -- Non-critical error, that can be given to the user in one form or another
    Critical  -- Critical error leading to application termination
    deriving (Eq, Enum, Ord, Show)

type ColorScheme = Color
type Enable = Bool
data LogSettings = LogSettings {colorScheme :: ColorScheme, debugMode :: Enable} deriving Show

-----------------------------Class---------------------------------------------
class Monad m => MLog m where
  getSettings :: m LogSettings
  setSettings :: ColorScheme -> Enable -> m ()
  getConfig :: m LogConfig
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

defaultSettings :: LogSettings
defaultSettings = LogSettings Black True

defaultConfig :: LogConfig
defaultConfig = LogConfig {colorEnable = False, terminalEnable = True, fileEnable = False, minLevel = 0}

logM :: (MLog m, Show a) => m a -> m a
logM m = do
    a <- m
    debugM a
    return a

-- * An exception has been made for debug information - it can be of any type Show a,
-- not just a String
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

-----------------------------Default implementation----------------------------
-- The default implementation of the MLog typeclass for the IO monad.
-- In pure code, for example for testing, you can replace this implementation with another one,
-- for example based on writerT, or empty return () implementation
-- Info can be shown in different color schemes, and for other levels the color corresponds to the level
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

            getColor :: LogLevel -> Color
            getColor  Debug    = Green
            getColor  Info     = Blue -- here you can use different color schemes for the convenience of displaying information
            getColor  Warn     = Magenta
            getColor  Error    = Yellow
            getColor  Critical = Red

            file :: (MonadIO m, ToJSON a) => a -> m()
            file str = do
                liftIO $ B.appendFile "log.txt" $ convert . encode $ str
                liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)





