module Upload where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import  qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Database.PostgreSQL.Simple.Time
import Class
import Types
import qualified Query 
import Query (q, (<+>), whereAll, inList)
-- import Data.Text
import Control.Monad.Except
import Control.Monad.Trans.Except
import Common
import Data.List
--import Database.PostgreSQL.Simple
import qualified Network.HTTP.Types.URI as HTTP
import Data.Maybe
import qualified Database.PostgreSQL.Simple.Types as SQL
import Data.Aeson hiding (encode)
import Control.Monad.Identity
import Transformer
import qualified Row
import qualified Select
import JSON
import Database.PostgreSQL.Simple
import Error 
import Parse
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import NeatInterpolation
import qualified Data.Text.IO as T
import qualified System.Console.ANSI as Color
import Text.Read
import qualified Params
import qualified Insert
import API
import Router
import Data.Aeson.Encode.Pretty
import qualified Delete
import qualified State as S
import qualified Update
import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import qualified Data.ByteString as B
import Data.Map as M ((!))

--при некорректном запросе с телом ответ с ошибкой не возвращается. При запросе без тела нормально возвращается
--при запросе get
--Error: write ECONNRESET

--запрос post то же самое

--загрузка фотографии на сервер
photo :: Request -> T Changed
photo req = do
    --params <- S.getParams 
    ParamEq (Str name) <- S.getParam "name"
    let path = "images/" <> name
    saveBinary req path
    S.getChanged 


saveBinary :: Request -> Path -> T ()
saveBinary req path = do
    -- chunk  <- toT $ getRequestBodyChunk  req 
    -- Log.debugT chunk
    
    let fileSize =  requestBodyLength req 
    let maxFileSize = 5*1024*1024
    case fileSize of 
        ChunkedBody -> throwT $ RequestError $ template "Неизвестный размер файла. Выберите файл меньше 5МБ ({0}Б)" [show maxFileSize]
        KnownLength  n| n >= maxFileSize -> throwT $ RequestError $ template "Размер файла составляет {0} Б . Выберите файл меньше 5 МБ ({1} Б)" [show n, show maxFileSize]
        _ -> return()
    toT $ B.writeFile path mempty
    toT $ stream 0 (getRequestBodyChunk req) 
    

    where
    stream :: Int -> IO B.ByteString -> T ()
    stream n str = do
        -- when (n > 100) $ do 
        -- throwT $ RequestError "Слишком большой размер файла!" -- какой? около 1,41МБ
        -- удалить, то что загрузили!!!
        bs <- toT str 
        --print bs
        if bs == mempty 
            then do
                putStrLnT $ template "Успешно записано {0} чаcтей файла" [show n] 
                S.addChanged Upload Photo 1
                --return () 
            else do
                toT $ B.appendFile path bs 
                -- print $ B.length bs
                -- print $ B.length "xxs"
                -- print $ B.length "яяя"
                -- print "xxs"
                -- print "яяя"
                -- undefined
                stream (n+1) str