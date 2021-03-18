--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Response where

import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import qualified System.Console.ANSI as Color

import Data.Aeson

--import qualified Data.ByteString.Lazy as L

import Types
import Class
import qualified Log
import qualified DB 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Common
import qualified Encode
import Transformer
import Error 
import Data.Maybe
import Control.Monad.Except
import Data.List
import Control.Monad.Trans.Except
--import Database.PostgreSQL.Simple
--import Network.HTTP.Types.URI
import Data.Aeson.Encode.Pretty


saveBinary :: Request -> T ()
saveBinary req = do
    -- chunk  <- toT $ getRequestBodyChunk  req 
    -- Log.debugT chunk
    toT $ B.writeFile "images/1.jpg" mempty
    toT $ stream 0 (getRequestBodyChunk req) where

    stream :: Int -> IO B.ByteString -> T()
    stream n str = do
        when (n > 100) $ do 
            throwT $ RequestError "Слишком большой размер файла!" -- какой? около 1,41МБ
            --удалить, то что загрузили!!!
        bs <- toT str 
        --print bs
        if bs == mempty 
            then do
                putStrLnT $ template "Успешно записано {0} чаcтей файла" [show n] 
                --return () 
            else do
                toT $ B.appendFile "images/1.jpg" bs
                -- print $ B.length bs
                -- print $ B.length "xxs"
                -- print $ B.length "яяя"
                -- print "xxs"
                -- print "яяя"
                -- undefined
                stream (n+1) str

    


get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    let pathInfo = Wai.pathInfo req
    let queryString = Wai.queryString req
    saveBinary req
    undefined
    json <- DB.getJSON (rawPathInfo req) pathInfo queryString
    Response.json json
    --Response.sendData (DB.execute (rawPathInfo req) pathInfo) pathInfo queryString 



    
    -- case pathInfo of 
    --     ["users"] -> Response.sendData DB.getUsers pathInfo queryString 
    --     ["authors"] -> Response.sendData DB.getAuthors pathInfo queryString 
    --     ["categories"] -> Response.sendData DB.getCategories pathInfo queryString 
    --     ["posts"] -> Response.sendData DB.getPosts pathInfo queryString
    --     ["tags"] -> Response.sendData DB.getTags pathInfo queryString
    --     ["insert_tag"] -> Response.sendData DB.insertTag pathInfo queryString
    --     ["edit_tag"] -> Response.sendData DB.editTag pathInfo queryString
    --     ["delete_tag"] -> Response.sendData DB.deleteTag pathInfo queryString

        -- _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]

-- sendData :: ( ToJSON a, Show a) => (Query -> T a) -> PathInfo -> Query  -> T Response
-- sendData tgetData pathInfo queryString  = do
--     Log.setSettings Color.Blue  True $ template "sendData: {0}" [show . head $ pathInfo] 
--     _data <- tgetData queryString
--     let edata = encodePretty _data
--     --Log.dataT Log.Info _data
--     Response.json edata


json :: LC.ByteString -> T Response
json = return . Wai.responseLBS status200 [(hContentType, "text/plain")] 

--здесь нужен другой статус, а не статус 200
--это чистая безошибочная функция
errorHandler :: E -> Response
errorHandler e = do
    --let err = convertL ("Ошибочка вышла"::String)
    Wai.responseLBS status200 [(hContentType, "text/plain")] (convertL . show $ e)

