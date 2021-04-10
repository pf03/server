module Upload where
{-# LANGUAGE FlexibleInstances #-}
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import  qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Database.PostgreSQL.Simple.Time
import Types
--import DB (q, (<+>), whereAll, inList)
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
import qualified Cache

--при некорректном запросе с телом ответ с ошибкой не возвращается. При запросе без тела нормально возвращается
--при запросе get
--Error: write ECONNRESET

--запрос post то же самое

--загрузка фотографии на сервер
photo :: (MIOError m, MCache m) => Request -> m ()
photo req = do
    --params <- Cache.getParams 
    ParamEq (Str name) <- Cache.getParam "name"
    let path = "images/" <> name
    saveBinary req path
    --Cache.getChanged 


saveBinary :: (MIOError m, MCache m) => Request -> Path -> m ()
saveBinary req path = do
    -- chunk  <- toT $ getRequestBodyChunk  req 
    -- Log.debugT chunk
    
    let fileSize =  requestBodyLength req 
    let maxFileSize = 5*1024*1024
    case fileSize of 
        ChunkedBody -> Error.throw $ RequestError $ template "Неизвестный размер файла. Выберите файл меньше 5МБ ({0}Б)" [show maxFileSize]
        KnownLength  n| n >= maxFileSize -> Error.throw $ RequestError $ template "Размер файла составляет {0} Б . Выберите файл меньше 5 МБ ({1} Б)" [show n, show maxFileSize]
        _ -> return()
    liftEIO $ B.writeFile path mempty
    stream 0 (getRequestBodyChunk req) 
    

    where
    stream :: (MIOError m, MCache m) => Int -> IO B.ByteString -> m ()
    stream n str = do
        -- when (n > 100) $ do 
        -- throwT $ RequestError "Слишком большой размер файла!" -- какой? около 1,41МБ
        -- удалить, то что загрузили!!!
        bs <- liftEIO str 
        --print bs
        if bs == mempty 
            then do
                putStrLnT $ template "Успешно записано {0} чаcтей файла" [show n] 
                Cache.addChanged Upload Photo 1
                --return () 
            else do
                liftEIO $ B.appendFile path bs 
                -- print $ B.length bs
                -- print $ B.length "xxs"
                -- print $ B.length "яяя"
                -- print "xxs"
                -- print "яяя"
                -- undefined
                stream (n+1) str

-----------------------------------------------------------------------------------------------------------------------
--для работы с телом запроса, поделенным на чанки. liftEIO более безопасно чем liftIO
stream :: (MonadIO m, Eq a, Monoid a) => IO a -> (a -> IO()) -> m ()
stream source receiver = liftIO $ helper 0 source receiver where
  helper n source receiver = do
      a <- source
      if a == mempty
        then putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
        else do
            receiver a
            helper (n+1) source receiver

--для работы с телом запроса, поделенным на чанки - должно быть не более одного чанка
streamOne :: (MIOError m, Eq a, Monoid a) => IO a -> m a
streamOne source = helper 0 source where
    helper n source = do
        liftIO $ putStrLn "streamOne"
        a <- liftEIO source
        if a == mempty
            then do
                liftIO $ putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
                return a
            else if n>=1
                then Error.throw $ RequestError "Слишком длинное тело запроса. Тело запроса должно состоять не более, чем из одного чанка"
                else (a <>) <$> helper (n + 1) source

--для работы с телом запроса, поделенным на чанки - должно быть пустым
streamEmpty :: (MIOError m, Eq a, Monoid a) => IO a -> m ()
streamEmpty source = do
    a <- liftEIO source
    if a == mempty
        then return ()
        else Error.throw $ RequestError "Тело запроса должно быть пустым"