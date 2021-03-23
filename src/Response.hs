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

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    -- let pathInfo = Wai.pathInfo req
    -- let queryString = Wai.queryString req
    -- saveBinary req
    --undefined
    -- json <- DB.getJSON (rawPathInfo req) pathInfo queryString
    json <- DB.getJSON_ req
    Response.json json

json :: LC.ByteString -> T Response
json = return . Wai.responseLBS status200 [(hContentType, "text/plain")] 

--для некоторых типов ошибки нельзя выводить текст, например ошибка конфига
errorHandler :: E -> Response
errorHandler e = Wai.responseLBS (getStatus e) [(hContentType, "text/plain")] (convertL . show $ e)

