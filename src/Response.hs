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

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    
    let pathInfo = Wai.pathInfo req
    getUsers
    case pathInfo of 
        ["users"] -> getUsers
        _ -> throwT . RequestError $ template  "Неизвестный путь: {0}"  [show . rawPathInfo $ req]
    --return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"




getUsers :: T Response
getUsers = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.textT Log.Info "Request.get"
    users <- DB.getUsers 
    -- let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
    let eusers = encode users
    Log.dataT Log.Info users
    return $ Wai.responseLBS status200 [(hContentType, "text/plain")] eusers


-- getUsersBody :: T LC.ByteString
-- getUsersBody = do
--     users <- DB.getUsers 
--     let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
--     let eusers = Encode.users users
--     Log.dataT Log.Info users
--     return eusers

--это чистая безошибочная функция
errorHandler :: E -> Response
errorHandler e = do
    --let err = convertL ("Ошибочка вышла"::String)
    Wai.responseLBS status200 [(hContentType, "text/plain")] (convertL . show $ e)