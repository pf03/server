module Response where

import Network.Wai (responseLBS, Application)
import Network.Wai.Internal ( Request, Response, ResponseReceived )
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import qualified System.Console.ANSI as Color

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

rdefault :: Response
rdefault = responseLBS status200 [(hContentType, "text/plain")] "Ошибочка вышла"

bs :: LC.ByteString 
bs = convertL ("здравствуйте" :: String)

get :: Request -> T Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    getUsers
    --return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"


getUsers :: T Response
getUsers = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.textT Log.Info "Request.get"
    users <- DB.getUsers 
    let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
    let eusers = Encode.users users
    Log.dataT Log.Info users
    return $ responseLBS status200 [(hContentType, "text/plain")] eusers


--удобная функция для отладки
getUsersBody :: T LC.ByteString
getUsersBody = do
    users <- DB.getUsers 
    let keyboard = Encode.keyboard ["Вася", "Петя", "Маша"] 
    let eusers = Encode.users users
    Log.dataT Log.Info users
    return eusers