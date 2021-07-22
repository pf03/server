module Color where

import Common.Functions (template)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleIntensity (BoldIntensity, NormalIntensity),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGR,
  )

-- | Set colors and write some text in those colors.
colorCell :: ColorIntensity -> ConsoleIntensity -> (String, Maybe Color) -> (String, Maybe Color) -> IO ()
colorCell colorIntensity consoleIntensity (colorName1, mColor1) (colorName2, mColor2) = do
  maybe (return ()) (\color1 -> setSGR [SetColor Foreground colorIntensity color1]) mColor1
  maybe (return ()) (\color2 -> setSGR [SetColor Background colorIntensity color2]) mColor2
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