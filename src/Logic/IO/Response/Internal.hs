module Logic.IO.Response.Internal where

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
import qualified Logic.Pure.Params.Functions as Params
import Network.HTTP.Types ( parseQuery )
import qualified Network.Wai.Internal as Wai

getJSON :: MTrans m => Wai.Request -> m LC.ByteString
getJSON request = do
  Cache.resetChanged
  let (rawPathInfo, pathInfo, queryString) = (Wai.rawPathInfo request, Wai.pathInfo request, Wai.queryString request)
  Auth.auth request
  auth <- Cache.getAuth
  api@(API apiType _) <- Log.withLogM $ API.router rawPathInfo pathInfo auth
  Cache.setAPI api
  requestBody <- case apiType of
    Upload -> return mempty
    _ -> Upload.streamOne (Wai.getRequestBodyChunk request)
  queryStringBody <- case apiType of
    Upload -> return []
    _ -> return $ parseQuery requestBody
  Log.writeDebugM requestBody
  params <-
    if apiType `elem` [Auth, Delete, Insert, Update]
      then Error.catch (Log.withLogM $ Params.parseParams api queryStringBody) $
        \(Error.RequestError err) ->
          Error.throwRequest
            "{0}.\n Attention: parameters for this request must be passed in the request body by the x-www-form-urlencoded method"
            [err]
      else Error.catch (Log.withLogM $ Params.parseParams api (queryString <> queryStringBody)) $
        \(Error.RequestError err) ->
          Error.throwRequest
            "{0}.\n Attention: parameters for this request can be passed both in the query string and in the request body by the x-www-form-urlencoded method"
            [err]
  Cache.setParams params
  evalJSON api request

evalJSON :: MTrans m => API -> Wai.Request -> m LC.ByteString
evalJSON api req = case api of
  API Auth [] -> encode Auth.login
  -- Files
  API Upload [Photo] -> encode $ Photos.upload req
  API Load [Image fileName] -> encode $ Photos.load fileName
  API Select [Photo] -> encode Photos.select
  -- API, which returns the number of changed entities in the DB
  API Insert [User] -> encode Insert.insertUser
  API Insert [Author] -> encode Insert.insertAuthor
  API Insert [Category] -> encode Insert.insertCategory
  API Insert [Tag] -> encode Insert.insertTag
  API Insert [Draft] -> encode Insert.insertDraft
  API Insert [Draft, Id n, Post] -> encode $ Insert.publish n
  API Insert [Post, Id n, Comment] -> encode $ Insert.insertComment n
  API Update [User, Id n] -> encode $ Update.updateUser n
  API Update [Author, Id n] -> encode $ Update.updateAuthor n
  API Update [Category, Id n] -> encode $ updateCategory n
  API Update [Tag, Id n] -> encode $ Update.updateTag n
  API Update [Draft, Id n] -> encode $ Update.updateDraft n
  API Update [Post, Id n] -> encode $ Update.updatePost n
  API Delete [User, Id n] -> encode $ Delete.deleteUser n
  API Delete [Author, Id n] -> encode $ Delete.deleteAuthor n
  API Delete [Category, Id n] -> encode $ Delete.deleteCategory n
  API Delete [Tag, Id n] -> encode $ Delete.deleteTag n
  API Delete [Post, Id n] -> encode $ Delete.deletePost n
  API Delete [Draft, Id n] -> encode $ Delete.deleteDraft n
  API Delete [Comment, Id n] -> encode $ Delete.deleteComment n
  -- API, which returns the json result from DB
  API Select [User] -> encode Select.selectUsers
  API Select [Author] -> encode $ JSON.evalAuthors <$> Select.selectAuthors
  API Select [Category] -> encode getCategories
  API Select [Tag] -> encode Select.selectTags
  API Select [Post] -> encode getPosts
  API Select [Draft] -> encode getDrafts
  API Select [Post, Id n, Comment] -> encode $ JSON.evalComments <$> Select.selectComments n
  API SelectById [User, Id n] -> encode $ Select.selectUser n
  API SelectById [Author, Id n] -> encode $ (JSON.evalAuthor <$>) <$> Select.selectAuthor n
  API SelectById [Category, Id n] -> encode $ getCategory n
  API SelectById [Tag, Id n] -> encode $ Select.selectTag n
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
  selectPosts <- Select.selectPosts
  JSON.evalPosts categories selectPosts

getDrafts :: MTrans m => m [JSON.Draft]
getDrafts = do
  categories <- getAllCategories
  selectDrafts <- Select.selectDrafts
  JSON.evalDrafts categories selectDrafts

getPost :: MTrans m => Int -> m (Maybe JSON.Post)
getPost paramId = do
  categories <- getAllCategories
  selectPosts <- Select.selectPost paramId
  jsonPosts <- JSON.evalPosts categories selectPosts
  return $ listToMaybe jsonPosts

getDraft :: MTrans m => Int -> m (Maybe JSON.Draft)
getDraft paramId = do
  categories <- getAllCategories
  selectDrafts <- Select.selectDraft paramId
  jsonDrafts <- JSON.evalDrafts categories selectDrafts
  return $ listToMaybe jsonDrafts

getAllCategories :: MTrans m => m [JSON.Category]
getAllCategories = do
  allCategories <- Select.selectAllCategories
  JSON.evalCategories allCategories allCategories

getCategories :: MTrans m => m [JSON.Category]
getCategories = do
  allCategories <- Select.selectAllCategories
  categories <- Select.selectCategories
  JSON.evalCategories allCategories categories

updateCategory :: MTrans m => Int -> m ()
updateCategory paramId = do
  params <- Cache.getParams
  allCategories <- Select.selectAllCategories
  JSON.checkCyclicCategory paramId params allCategories
  Update.updateCategory paramId

getCategory :: MTrans m => Int -> m (Maybe JSON.Category)
getCategory paramId = do
  allCats <- Select.selectAllCategories
  mCategory <- Select.selectCategory paramId
  sequenceA $ JSON.evalCategory allCats <$> mCategory