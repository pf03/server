--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
--importPriority = 20
module Transformer where 
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import qualified State as S

import System.Console.ANSI

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--наш проект
import qualified Config --40
-- import Error
import Types  --100
-- import Parse
import Error
import qualified Log
import Class
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple



--Пользователи ничего не должны знать о внутренней структуре нашего трансформера.
--Для них есь три сущности - class ToTransformer, функции toT, runT ну и несколько вспомогательныйх функций типа printT

throwT :: E -> T a
throwT e  = toT (throwE e::Except E a)  



--запуск основного трансформера и всех монад попроще
--трансформер есть кусок программы, который можно запускать независимо от основной программы,
--поэтому в runT уже имеется и считывание config, и подключение к БД, и т. д.
runT :: (ToTransformer m, Show a) => m a -> IO()
runT m = do 
    let settings = Log.LogSettings Cyan True "runT"
    es <- runExceptT Config.readS
    case es of 
        Left e -> do
            let dlc = Log.defaultConfig
            Log.text dlc settings Log.Error "Ошибка запуска трансформера: "
            Log.error dlc settings e
        Right s -> do 
            let cl = configLog s
            Log.text cl settings Log.Info "Конфиг успешно считан, бд успешно подключена..."
            ea <- runExceptT $ runStateT (toT m) s
            case ea  of
                Left e -> do 
                    Log.text cl settings Log.Error "Ошибка приложения: "
                    Log.error cl settings e
                Right a -> do 
                    Log.text cl settings Log.Info "Результат: "
                    Log.ldata cl settings Log.Data $ fst a



--сохранять конфиг не надо в этом проекте
-- saveST :: T()
-- saveST = do
--     --v <- toT readConfigValue 
--     s <- get
--     --let bc = encodeConfig v config
--     toT $ saveS s



testLog :: IO()
testLog = runT $ do
    Log.dataT Log.Debug $ "Debug data value " ++ show [1..10]  :: T()
    Log.dataT Log.Info $ "Info data value " ++ show [1..10] 
    Log.dataT Log.Error $ "Error data value " ++ show [1..10] 
    Log.dataT Log.Data $ "Data data value " ++ show [1..10] 
    Log.dataT Log.Warning  $ "Warning data value " ++ show [1..10] 
    Log.colorTextT Blue Log.Debug $"Blue color scheme " ++ klichko
    Log.colorTextT Cyan Log.Debug $ "Cyan color scheme " ++ klichko
    Log.colorTextT Green Log.Debug $ "Green color scheme " ++ klichko
    Log.colorTextT Yellow Log.Debug $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"

---------------------------------------MonadLog-------------------------------------------------------
