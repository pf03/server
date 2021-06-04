module Logic.IO.Response where

import Common.Convert (Convert (convert), ConvertL (convertL))
import Common.Functions (splitOnLast)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe (listToMaybe)
import Data.Typeable (Typeable, typeOf)
import Interface.Class (MTrans)
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types as Cache
  ( API (..),
    APIType (Author, Category, Comment, Draft, Id, Image, Photo, Post, Tag, User),
    QueryType (Auth, Delete, Insert, Load, Select, SelectById, Update, Upload),
  )
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.DB.Auth as Auth
import qualified Logic.DB.Delete as Delete
import qualified Logic.DB.Insert as Insert
import qualified Logic.DB.Select.Exports as Select
import qualified Logic.DB.Update as Update
import qualified Logic.IO.File as File
import qualified Logic.IO.Photos as Photos
import qualified Logic.IO.Upload as Upload
import qualified Logic.Pure.API as API
import qualified Logic.Pure.JSON.Exports as JSON
import qualified Logic.Pure.Params as Params
import Network.HTTP.Types (hContentType, internalServerError500, parseQuery, status200)
import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
  ( Request (pathInfo, queryString, rawPathInfo),
    Response,
    getRequestBodyChunk,
  )

get :: MTrans m => Request -> m Response
get req = do
  Log.debugM req
  json <- getJSON req
  api <- Cache.getAPI
  case api of
    API Load [Image fn] -> do
      let (_, extention) = splitOnLast '.' fn
      return $
        Wai.responseFile
          status200
          [(hContentType, "image/" <> convert extention)]
          (Photos.photosPath <> "/" <> fn)
          Nothing
    _ -> return $ Wai.responseLBS status200 [(hContentType, "text/plain")] json

errorHandler :: Error.Error -> Response
errorHandler e = do
  let status = Error.getStatus e
  let text =
        if status == internalServerError500
          then "Internal server error"
          else convertL $ show e
  Wai.responseLBS status [(hContentType, "text/plain")] text

getJSON :: MTrans m => Request -> m LC.ByteString
getJSON req = do
  Cache.resetChanged
  let (rpinfo, pinfo, qs) = (Wai.rawPathInfo req, Wai.pathInfo req, Wai.queryString req)
  Auth.auth req
  a <- Cache.getAuth
  api@(API apiType _) <- Log.logM $ API.router rpinfo pinfo a
  Cache.setAPI api
  rb <- case apiType of
    Upload -> return mempty
    _ -> Upload.streamOne (getRequestBodyChunk req)
  qsBody <- case apiType of
    Upload -> return []
    _ -> return $ parseQuery rb
  Log.debugM rb
  params <-
    if apiType `elem` [Auth, Delete, Insert, Update]
      then Error.catch (Log.logM $ Params.parseParams api qsBody) $
        \(Error.RequestError e) ->
          Error.throwRequest
            "{0}.\n Attention: parameters for this request must be passed in the request body by the x-www-form-urlencoded method"
            [e]
      else Error.catch (Log.logM $ Params.parseParams api (qs <> qsBody)) $
        \(Error.RequestError e) ->
          Error.throwRequest
            "{0}.\n Attention: parameters for this request can be passed both in the query string and in the request body by the x-www-form-urlencoded method"
            [e]
  Cache.setParams params
  evalJSON api req

evalJSON :: MTrans m => API -> Request -> m LC.ByteString
evalJSON api req = case api of
  API Auth [] -> encode Auth.login
  -- Files
  API Upload [Photo] -> encode $ Photos.upload req
  API Load [Image fn] -> encode $ Photos.load fn
  API Select [Photo] -> encode Photos.select
  -- API, which returns the number of changed entities in the DB
  API Insert [User] -> encode Insert.user
  API Insert [Author] -> encode Insert.author
  API Insert [Category] -> encode Insert.category
  API Insert [Tag] -> encode Insert.tag
  API Insert [Draft] -> encode Insert.draft
  API Insert [Draft, Id n, Post] -> encode $ Insert.publish n
  API Insert [Post, Id n, Comment] -> encode $ Insert.comment n
  API Update [User, Id n] -> encode $ Update.user n
  API Update [Author, Id n] -> encode $ Update.author n
  API Update [Category, Id n] -> encode $ updateCategory n
  API Update [Tag, Id n] -> encode $ Update.tag n
  API Update [Draft, Id n] -> encode $ Update.draft n
  API Update [Post, Id n] -> encode $ Update.post n
  API Delete [User, Id n] -> encode $ Delete.user n
  API Delete [Author, Id n] -> encode $ Delete.author n
  API Delete [Category, Id n] -> encode $ Delete.category n
  API Delete [Tag, Id n] -> encode $ Delete.tag n
  API Delete [Post, Id n] -> encode $ Delete.post n
  API Delete [Draft, Id n] -> encode $ Delete.draft n
  API Delete [Comment, Id n] -> encode $ Delete.comment n
  -- API, which returns the json result from DB
  API Select [User] -> encode Select.users
  API Select [Author] -> encode $ JSON.evalAuthors <$> Select.authors
  API Select [Category] -> encode getCategories
  API Select [Tag] -> encode Select.tags
  API Select [Post] -> encode getPosts
  API Select [Draft] -> encode getDrafts
  API Select [Post, Id n, Comment] -> encode $ JSON.evalComments <$> Select.comments n
  API SelectById [User, Id n] -> encode $ Select.user n
  API SelectById [Author, Id n] -> encode $ (JSON.evalAuthor <$>) <$> Select.author n
  API SelectById [Category, Id n] -> encode $ getCategory n
  API SelectById [Tag, Id n] -> encode $ Select.tag n
  API SelectById [Post, Id n] -> encode $ getPost n
  API SelectById [Draft, Id n] -> encode $ getDraft n
  _ -> Error.throw $ Error.patError "Response.evalJSON" api

encode :: MTrans m => (Typeable a, ToJSON a) => m a -> m LC.ByteString
encode ta = do
  a <- ta
  json <- if showType a == "()" then encodePretty <$> Cache.getChanged else encodePretty <$> ta
  File.writeResponseJSON json
  return json

showType :: Typeable a => a -> String
showType = show . typeOf

getPosts :: MTrans m => m [JSON.Post]
getPosts = do
  categories <- getAllCategories
  _ <- Cache.modifyParamsM $ JSON.evalParams categories
  selectPosts <- Select.posts
  JSON.evalPosts categories selectPosts

getDrafts :: MTrans m => m [JSON.Draft]
getDrafts = do
  categories <- getAllCategories
  selectDrafts <- Select.drafts
  JSON.evalDrafts categories selectDrafts

getPost :: MTrans m => Int -> m (Maybe JSON.Post)
getPost pid = do
  categories <- getAllCategories
  selectPosts <- Select.post pid
  jsonPosts <- JSON.evalPosts categories selectPosts
  return $ listToMaybe jsonPosts

getDraft :: MTrans m => Int -> m (Maybe JSON.Draft)
getDraft pid = do
  categories <- getAllCategories
  selectDrafts <- Select.draft pid
  jsonDrafts <- JSON.evalDrafts categories selectDrafts
  return $ listToMaybe jsonDrafts

getAllCategories :: MTrans m => m [JSON.Category]
getAllCategories = do
  allCategories <- Select.allCategories
  JSON.evalCategories allCategories allCategories

getCategories :: MTrans m => m [JSON.Category]
getCategories = do
  allCategories <- Select.allCategories
  categories <- Select.categories
  JSON.evalCategories allCategories categories

updateCategory :: MTrans m => Int -> m ()
updateCategory pid = do
  params <- Cache.getParams
  allCategories <- Select.allCategories
  JSON.checkCyclicCategory pid params allCategories
  Update.category pid

getCategory :: MTrans m => Int -> m (Maybe JSON.Category)
getCategory pid = do
  allCats <- Select.allCategories
  mcat <- Select.category pid
  sequenceA $ JSON.evalCategory allCats <$> mcat