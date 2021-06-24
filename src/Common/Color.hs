module Common.Color where

import Common.Functions (ifJust, template)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleIntensity (BoldIntensity, NormalIntensity),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGR,
  )

setSchemeT :: MonadIO m => Color -> m ()
setSchemeT color = case color of
  Blue -> setColorSchemeT Dull BoldIntensity (Just Yellow) (Just Blue)
  Cyan -> setColorSchemeT Vivid NormalIntensity (Just Black) (Just Cyan)
  Green -> setColorSchemeT Dull BoldIntensity (Just Blue) (Just Green)
  Yellow -> setColorSchemeT Vivid NormalIntensity (Just Blue) (Just Yellow)
  _ -> resetColorSchemeT

setColorT :: MonadIO m => Color -> m ()
setColorT color = setColorSchemeT Vivid NormalIntensity (Just color) Nothing

setColorSchemeT :: (MonadIO m) => ColorIntensity -> ConsoleIntensity -> Maybe Color -> Maybe Color -> m ()
setColorSchemeT colorIntensity consoleIntensity mColor1 mColor2 = liftIO $ do
  ifJust mColor1 $ setSGR [SetColor Foreground colorIntensity (fromJust mColor1)]
  ifJust mColor2 $ setSGR [SetColor Background colorIntensity (fromJust mColor2)]
  setSGR [SetConsoleIntensity consoleIntensity]

resetColorSchemeT :: MonadIO m => m ()
resetColorSchemeT = liftIO $ setSGR [Reset]

-----------------------------Testing table-------------------------------------

-- | Set colors and write some text in those colors.
colorCell :: ColorIntensity -> ConsoleIntensity -> (String, Maybe Color) -> (String, Maybe Color) -> IO ()
colorCell colorIntensity consoleIntensity (colorName1, mColor1) (colorName2, mColor2) = do
  ifJust mColor1 $ setSGR [SetColor Foreground colorIntensity (fromJust mColor1)]
  ifJust mColor2 $ setSGR [SetColor Background colorIntensity (fromJust mColor2)]
  setSGR [SetConsoleIntensity consoleIntensity]
  putStr $ template " {0} {1} " [take 3 colorName1, take 3 colorName2]
  setSGR [Reset] -- Reset to default colour scheme

colors :: [Maybe Color]
colors = [Nothing] <> map Just [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]

colorNames :: [String]
colorNames = ["Default", "Black", "Red", "Green", "Yellow", "Blue", "Magenta", "Cyan", "White"]

colorPairs :: [(String, Maybe Color)]
colorPairs = zip colorNames colors

colorRow :: ColorIntensity -> ConsoleIntensity -> (String, Maybe Color) -> IO ()
colorRow colorIntensity consoleIntensity (colorName1, mColor1) = do
  mapM_ (colorCell colorIntensity consoleIntensity (colorName1, mColor1)) colorPairs
  putStrLn ""

colorTable :: IO ()
colorTable = do
  putStrLn "Color Table Dull - BoldIntensity ..."
  mapM_ (colorRow Dull BoldIntensity) colorPairs
  putStrLn "Color Table Vivid - BoldIntensity ..."
  mapM_ (colorRow Vivid BoldIntensity) colorPairs
  putStrLn "Color Table Dull - NormalIntensity ..."
  mapM_ (colorRow Dull NormalIntensity) colorPairs
  putStrLn "Color Table Vivid - NormalIntensity ..."
  mapM_ (colorRow Vivid NormalIntensity) colorPairs