module Log where

import qualified Interface.MLog.Exports as Log
import qualified Transformer.Exports as Transformer

testLog :: IO ()
testLog = Transformer.runT $ do
  Log.writeDebugM $ "Debug data value " ++ show [1 .. 10 :: Int]
  Log.writeInfoM $ "Info data value " ++ show [1 .. 10 :: Int]
  Log.writeWarnM $ "warnM data value " ++ show [1 .. 10 :: Int]
  Log.writeErrorM $ "Error data value " ++ show [1 .. 10 :: Int]
  Log.writeCriticalM $ "criticalM data value " ++ show [1 .. 10 :: Int]
  Log.writeInfoColorM Log.BlueScheme $ "Blue color scheme " ++ klichko
  Log.writeInfoColorM Log.CyanScheme $ "Cyan color scheme " ++ klichko
  Log.writeInfoColorM Log.GreenScheme $ "Green color scheme " ++ klichko
  Log.writeInfoColorM Log.YellowScheme $ "Yellow color scheme " ++ klichko
  where
    klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"