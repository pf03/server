module Response where

import Network.Wai (responseLBS, Application)
import Network.Wai.Internal ( Request, Response, ResponseReceived )
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Types
import Class
import qualified Log

rdefault :: Response
rdefault = responseLBS status200 [(hContentType, "text/plain")] "Ошибочка вышла"

get :: Request -> T Response
get req = do
    Log.textT Log.Info "Request.get"
    return $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

