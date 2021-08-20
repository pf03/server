module Common.Color where

import Control.Monad.IO.Class (MonadIO (..))
import Interface.MLog.Types (ColorScheme (..))
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleIntensity (BoldIntensity, NormalIntensity),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGR,
  )

setScheme :: MonadIO m => ColorScheme -> m ()
setScheme color = case color of
  BlueScheme -> setSchemeColors Dull BoldIntensity (Just Yellow) (Just Blue)
  CyanScheme -> setSchemeColors Vivid NormalIntensity (Just Black) (Just Cyan)
  GreenScheme -> setSchemeColors Dull BoldIntensity (Just Blue) (Just Green)
  YellowScheme -> setSchemeColors Vivid NormalIntensity (Just Blue) (Just Yellow)
  BlackScheme -> resetColorScheme

setSchemeColors :: (MonadIO m) => ColorIntensity -> ConsoleIntensity -> Maybe Color -> Maybe Color -> m ()
setSchemeColors colorIntensity consoleIntensity mColor1 mColor2 = liftIO $ do
  maybe (return ()) (\color1 -> setSGR [SetColor Foreground colorIntensity color1]) mColor1
  maybe (return ()) (\color2 -> setSGR [SetColor Background colorIntensity color2]) mColor2
  setSGR [SetConsoleIntensity consoleIntensity]

setColor :: MonadIO m => Color -> m ()
setColor color = setSchemeColors Vivid NormalIntensity (Just color) Nothing

resetColorScheme :: MonadIO m => m ()
resetColorScheme = liftIO $ setSGR [Reset]