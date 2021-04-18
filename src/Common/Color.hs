module Common.Color 
    ( module Common.Color
    ) where 
--этот модуль должен импортироваться только модулем Log

import System.Console.ANSI
import Control.Monad.IO.Class
import Data.Maybe

--наши модули
import Common.Misc

setSchemeT :: MonadIO m => Color -> m ()
setSchemeT color = case color of
    Blue -> setColorSchemeT Dull BoldIntensity (Just Yellow) (Just Blue) 
    Cyan -> setColorSchemeT Vivid NormalIntensity (Just Black) (Just Cyan) 
    Green -> setColorSchemeT Dull BoldIntensity (Just Blue) (Just Green) 
    Yellow -> setColorSchemeT Vivid NormalIntensity (Just Blue) (Just Yellow) 
    _ -> resetColorSchemeT

setColorT ::  MonadIO m => Color -> m ()
setColorT color = setColorSchemeT Vivid NormalIntensity (Just color) Nothing 

--вообще coli может быть разным для Background и Foreground
setColorSchemeT :: (MonadIO m) => ColorIntensity -> ConsoleIntensity -> Maybe Color -> Maybe Color -> m()
setColorSchemeT coli coni mcolor1 mcolor2 = liftIO $ do
    ifJust mcolor1 $ setSGR [SetColor Foreground coli (fromJust mcolor1)]
    ifJust mcolor2 $ setSGR [SetColor Background coli (fromJust mcolor2)]
    setSGR [SetConsoleIntensity coni]

resetColorSchemeT ::  MonadIO m => m ()
resetColorSchemeT =  liftIO $ setSGR [Reset]

---------------------------TESTING TABLE-------------------------------------------------------------------

-- Set colors and write some text in those colors.
colorCell :: ColorIntensity -> ConsoleIntensity -> (String, Maybe Color) -> (String, Maybe Color) -> IO ()
colorCell coli coni (colorName1, mcolor1)  (colorName2, mcolor2) = do
    ifJust mcolor1 $ setSGR [SetColor Foreground coli (fromJust mcolor1)]
    ifJust mcolor2 $ setSGR [SetColor Background coli (fromJust mcolor2)]
    setSGR [SetConsoleIntensity coni]
    --putStr $ template (to18 "{0}-On-{1}") [colorName1, colorName2]
    putStr $ template  " {0} {1} " [take 3 colorName1, take 3 colorName2]
    setSGR [Reset]  -- Reset to default colour scheme
    --putStrLn "Default colors."

colors :: [Maybe Color]
colors = [Nothing] <> map Just [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]
colorNames :: [String]
colorNames = ["Default", "Black", "Red", "Green", "Yellow","Blue","Magenta","Cyan","White"]

colorPairs :: [(String, Maybe Color)]
colorPairs = zip colorNames colors

colorRow :: ColorIntensity ->  ConsoleIntensity ->(String, Maybe Color) -> IO ()
colorRow coli coni (c1, n1) = do
    mapM_ (colorCell coli coni (c1, n1)) colorPairs
    putStrLn ""

colorTable:: IO()
colorTable = do 
    putStrLn "Color Table Dull - BoldIntensity ..."
    mapM_ (colorRow Dull BoldIntensity) colorPairs
    putStrLn "Color Table Vivid - BoldIntensity ..."
    mapM_ (colorRow Vivid BoldIntensity) colorPairs
    putStrLn "Color Table Dull - NormalIntensity ..."
    mapM_ (colorRow Dull NormalIntensity) colorPairs
    putStrLn "Color Table Vivid - NormalIntensity ..."
    mapM_ (colorRow Vivid NormalIntensity) colorPairs