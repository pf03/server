module Interface.MLog.Functions where

import qualified Common.Color as Color
import Common.Convert (Convert (convert))
import Common.Functions (putStrLnT)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Interface.MLog.Class (MLog (..))
import Interface.MLog.Types
  ( ColorScheme (BlackScheme),
    Config (..),
    Level (..),
    Settings (Settings),
  )
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow))

--import Prelude hiding (error)

-----------------------------MLog----------------------------------------------
debugOff :: MLog m => m ()
debugOff = do
  Settings colorScheme _ <- getSettings
  setSettings colorScheme False

debugOn :: MLog m => m ()
debugOn = do
  Settings colorScheme _ <- getSettings
  setSettings colorScheme True

setColorScheme :: MLog m => ColorScheme -> m ()
setColorScheme colorScheme = do
  Settings _ logEnable <- getSettings
  setSettings colorScheme logEnable

getConfigSettings :: MLog m => m (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

resetSettings :: MLog m => m ()
resetSettings = do
  let Settings colorScheme logEnable = defaultSettings
  setSettings colorScheme logEnable

defaultSettings :: Settings
defaultSettings = Settings BlackScheme True

defaultConfig :: Config
defaultConfig =
  Config
    { configColorEnabled = False,
      configTerminalEnabled = True,
      configFileEnabled = False,
      configMinLevel = 0
    }

withLogM :: (MLog m, Show a) => m a -> m a
withLogM m = do
  a <- m
  writeDebugM a
  return a

-- * An exception has been made for debug information - it can be of any type Show a, not just a String

writeDebugM :: (MLog m, Show a) => a -> m ()
writeDebugM a = writeMessageM Debug (show a)

writeInfoM :: MLog m => String -> m ()
writeInfoM = writeMessageM Info

writeInfoColorM :: MLog m => ColorScheme -> String -> m ()
writeInfoColorM colorScheme str = do
  setColorScheme colorScheme
  writeInfoM str

writeWarnM :: MLog m => String -> m ()
writeWarnM = writeMessageM Warn

writeErrorM :: MLog m => String -> m ()
writeErrorM = writeMessageM Error

writeCriticalM :: MLog m => String -> m ()
writeCriticalM = writeMessageM Critical

writeMessageM :: MLog m => Level -> String -> m ()
writeMessageM level str = do
  (config, settings) <- getConfigSettings
  message config settings level str

-----------------------------MonadIO-------------------------------------------
writeDebug :: (MonadIO m, Show a) => Config -> Settings -> a -> m ()
writeDebug logConfig logSettings a = writeMessageIO logConfig logSettings Debug (show a)

writeInfo :: MonadIO m => Config -> Settings -> String -> m ()
writeInfo logConfig logSettings = writeMessageIO logConfig logSettings Info

writeInfoColor :: MonadIO m => Config -> ColorScheme -> String -> m ()
writeInfoColor logConfig colorScheme = writeMessageIO logConfig (Settings colorScheme False) Info

writeWarn :: MonadIO m => Config -> Settings -> String -> m ()
writeWarn logConfig logSettings = writeMessageIO logConfig logSettings Warn

writeError :: MonadIO m => Config -> Settings -> String -> m ()
writeError logConfig logSettings = writeMessageIO logConfig logSettings Error

writeCritical :: MonadIO m => Config -> Settings -> String -> m ()
writeCritical logConfig logSettings = writeMessageIO logConfig logSettings Critical

-----------------------------Default implementation----------------------------
-- The default implementation of the MLog type class for the IO monad.
-- In pure code, for example for testing, you can replace this implementation with another one,
-- for example based on writerT, or empty return () implementation
-- Info can be shown in different color schemes, and for other levels the color corresponds to the level
writeMessageIO :: MonadIO m => Config -> Settings -> Level -> String -> m ()
writeMessageIO (Config colorEnabled terminalEnabled fileEnabled minLevel) (Settings colorScheme debugMode) level text = do
  if level < toEnum minLevel && not debugMode
    then return ()
    else do
      when (colorEnabled && terminalEnabled) $ do
        if level == Info
          then Color.setScheme colorScheme
          else Color.setColor $ getColor level
      when terminalEnabled $ putStrLnT logText
      when fileEnabled $ writeToFile logText
      when (colorEnabled && terminalEnabled) Color.resetColorScheme
  where
    logText :: String
    logText = map toUpper (show level) <> " " <> text

    getColor :: Level -> Color
    getColor Debug = Green
    getColor Info = Blue -- here you can use different color schemes for the convenience of displaying information
    getColor Warn = Magenta
    getColor Error = Yellow
    getColor Critical = Red

    writeToFile :: MonadIO m => String -> m ()
    writeToFile str = do
      liftIO $ B.appendFile "log.txt" $ convert str
      liftIO $ B.appendFile "log.txt" "\n"