module Interface.MLog.Functions where

import qualified Common.Color as Color
import Common.Convert (Convert (convert))
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

-----------------------------MLog----------------------------------------------
setColorScheme :: MLog m => ColorScheme -> m ()
setColorScheme colorScheme = do
  Settings _ logEnable <- getSettings
  setSettings colorScheme logEnable

getConfigSettings :: MLog m => m (Config, Settings)
getConfigSettings = do
  config <- getConfig
  settings <- getSettings
  return (config, settings)

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
writeInfo :: MonadIO m => Config -> Settings -> String -> m ()
writeInfo logConfig logSettings = writeMessageIO logConfig logSettings Info

writeInfoColor :: MonadIO m => Config -> ColorScheme -> String -> m ()
writeInfoColor logConfig colorScheme = writeMessageIO logConfig (Settings colorScheme False) Info

writeError :: MonadIO m => Config -> Settings -> String -> m ()
writeError logConfig logSettings = writeMessageIO logConfig logSettings Error

writeCritical :: MonadIO m => Config -> Settings -> String -> m ()
writeCritical logConfig logSettings = writeMessageIO logConfig logSettings Critical

-----------------------------Default implementation----------------------------
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
      when terminalEnabled $ liftIO . putStrLn $ logText
      when fileEnabled $ writeToFile logText
      when (colorEnabled && terminalEnabled) Color.resetColorScheme
  where
    logText :: String
    logText = map toUpper (show level) <> " " <> text

    getColor :: Level -> Color
    getColor Debug = Green
    getColor Info = Blue
    getColor Warn = Magenta
    getColor Error = Yellow
    getColor Critical = Red

    writeToFile :: MonadIO m => String -> m ()
    writeToFile str = do
      liftIO $ B.appendFile "log.txt" $ convert str
      liftIO $ B.appendFile "log.txt" "\n"