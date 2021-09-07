module Logic.IO.Response.Functions where

import Common.Convert (Convert (convert), ConvertL (convertL))
import Common.Functions (breakOnLast)
import Interface.Class (MTrans)
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types as Cache (API (API), APIType (Image), QueryType (Load))
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Photos as Photos
import Logic.IO.Response.Internal (getJSON)
import Network.HTTP.Types (hContentType, internalServerError500, status200)
import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai (Request, Response)

get :: MTrans m => Request -> m Response
get req = do
  Log.writeDebugM req
  json <- getJSON req
  api <- Cache.getAPI
  case api of
    API Load [Image fn] -> do
      let (_, extension) = breakOnLast '.' fn
      return $
        Wai.responseFile
          status200
          [(hContentType, "image/" <> convert extension)]
          (Photos.photosPath <> "/" <> fn)
          Nothing
    _ -> return $ Wai.responseLBS status200 [(hContentType, "text/plain")] json

errorHandler :: Error.Error -> Response
errorHandler err = do
  let status = Error.getStatus err
  let text =
        if status == internalServerError500
          then "Internal server error"
          else convertL $ show err
  Wai.responseLBS status [(hContentType, "text/plain")] text