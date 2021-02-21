--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Response where

import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified System.Console.ANSI as Color

import Data.Aeson

--import qualified Data.ByteString.Lazy as L

import Types
import Class
import qualified Log
import qualified DB 

--import qualified Data.ByteString as B
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
import Network.HTTP.Types.URI

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    let pathInfo = Wai.pathInfo req
    let queryString = Wai.queryString req
    case pathInfo of 
        ["users"] -> Response.sendData DB.getUsers pathInfo queryString 
        ["authors"] -> Response.sendData DB.getAuthors pathInfo queryString 
        ["categories"] -> Response.sendData DB.getCategories pathInfo queryString 
        ["posts"] -> Response.sendData DB.getPosts pathInfo queryString
        ["tags"] -> Response.sendData DB.getTags pathInfo queryString
        _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]

sendData :: ( ToJSON a, Show a) => (Query -> T a) -> PathInfo -> Query  -> T Response
sendData tgetData pathInfo queryString  = do
    Log.setSettings Color.Blue  True $ template "sendData: {0}" [show . head $ pathInfo] 
    _data <- tgetData queryString
    let edata = encode _data
    Log.dataT Log.Info _data
    Response.json edata


json :: LC.ByteString -> T Response
json = return . Wai.responseLBS status200 [(hContentType, "text/plain")] 

--здесь нужен другой статус, а не статус 200
--это чистая безошибочная функция
errorHandler :: E -> Response
errorHandler e = do
    --let err = convertL ("Ошибочка вышла"::String)
    Wai.responseLBS status200 [(hContentType, "text/plain")] (convertL . show $ e)