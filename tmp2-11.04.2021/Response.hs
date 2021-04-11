--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Response where

import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentType)
import qualified System.Console.ANSI as Color

import Data.Aeson hiding (encode)

--import qualified Data.ByteString.Lazy as L

import Types
import qualified Log
import DB (MT, MDB) 
import qualified DB

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Common
import Transformer
import Data.Maybe
import Control.Monad.Except
import Data.List
import Control.Monad.Trans.Except
--import Database.PostgreSQL.Simple
--import Network.HTTP.Types.URI
import Data.Aeson.Encode.Pretty
import qualified API
import qualified Params
import qualified State as S
import API
import qualified JSON
import qualified Network.HTTP.Types.URI as HTTP
import qualified Upload
import qualified Auth
import qualified Select
import qualified Update
import qualified Delete
import qualified Insert
import qualified Cache
import qualified Error
import Error (MError)

import Cache

import Data.Typeable
import qualified File
import Types.API

-- можно сделать дополнительно MRequest для монады, которая имеет доступ к запросу, либо сделать ее часть Cache,
-- сто нелогично, так как Cache меняется, а Request нет. И request нужен не везде, где нужен Cache.
get :: MT m => Request -> m Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    json <- Response.getJSON_ req
    Response.json json

json :: Monad m => LC.ByteString -> m Response
json = return . Wai.responseLBS status200 [(hContentType, "text/plain")] 

--для некоторых типов ошибки нельзя выводить текст, например ошибка конфига
errorHandler :: E -> Response
errorHandler e = do
    let status = Error.getStatus e
    let text = if status == internalServerError500 
        then convertL ("Внутренняя ошибка сервера" :: String) --проверить (вообще есть ли такие ошибки? как правило сервер просто не запускается в таких случаях)
        else convertL . show $ e
    Wai.responseLBS status [(hContentType, "text/plain")] text



---------------------------------- бывший модуль DB---------------------------------------------------------------
getJSON_ :: MT m => Request -> m LC.ByteString
getJSON_ req = do
    Cache.resetChanged
    Log.setSettings Color.Blue True "Response.getJSONwithUpload" 
    Log.funcT Log.Debug "..."
    let (rawPathInfo, pathInfo, qs) = ( Wai.rawPathInfo req, Wai.pathInfo req, Wai.queryString req)

    requestBody <- Upload.streamOne (getRequestBodyChunk req)
    Log.debugT requestBody
    --эту функцию можно использовать для тестирования и эмуляции запросов
    let qsBody = parseQuery requestBody
    --Log.debugT req  
    --logT $ Auth.auth req --нужна ли эта функция logT, она вызывает ошибку

    Auth.auth req
    auth <- Cache.getAuth

    api@(API apiType queryTypes) <- Log.logM $ router rawPathInfo pathInfo auth
    
    params <- if apiType `elem` [Auth, Delete, Insert, Update] 
        then Error.catch (Log.logM $ Params.parseParams qsBody api) $
            \(RequestError e) -> Error.throw $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в теле запроса методом x-www-form-urlencoded" [e] 
        else Error.catch (Log.logM $ Params.parseParams qs api) $
            \(RequestError e) -> Error.throw $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в строке запроса" [e] 
    Cache.setParams params
    getJSON api req

--эта обертка для удобства тестирования--эту обертку перенести в Test
getJSONTest :: MT m => BC.ByteString -> PathInfo -> HTTP.Query -> HTTP.Query -> RequestHeaders -> m LC.ByteString
getJSONTest rawPathInfo pathInfo qs qsBody headers = do
    Cache.resetCache --это нужно только для тестов, в сервере и так трансформер возрождается заново при каждом запросе
    --Log.setSettings Color.Blue True "Response.getJSONTest" 
    --Log.funcT Log.Debug "..."
    let req  = Wai.defaultRequest {requestHeaders = headers}
    Auth.auth req
    auth <- Cache.getAuth
    api@(API apiType queryTypes) <- Log.logM $ router rawPathInfo pathInfo auth
    params <- if apiType `elem` [Auth, Delete, Insert, Update] 
        then Error.catch (Log.logM $ Params.parseParams qsBody api) $
            \(RequestError e) -> Error.throw $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в теле запроса методом x-www-form-urlencoded" [e] 
        else Error.catch (Log.logM $ Params.parseParams qs api) $
            \(RequestError e) -> Error.throw $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в строке запроса" [e] 
    Cache.setParams params
    getJSON api req

getJSON :: MT m => API -> Request -> m LC.ByteString
getJSON api req = case api of
    API Auth [] -> encode Auth.login

    API Upload [Photo] -> encode $ Upload.photo req

    --апи, которые не возвращают количество измененных строк
    --может publish сделать отдельным querytype?
    API Insert [User] -> encode Insert.user
    API Insert [Author] -> encode Insert.author
    API Insert [Category] -> encode Insert.category
    API Insert [Tag] -> encode Insert.tag
    API Insert [Draft] -> encode Insert.draft
    API Insert [Draft, Id n, Post] -> encode $ Insert.publish n
    API Insert [Post, Id n, Comment] -> encode $ Insert.comment n

    API Update [User, Id n] -> encode $ Update.user n
    API Update [Author, Id n] -> encode $ Update.author n
    API Update [Category, Id n] -> encode $ Response.updateCategory n
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

    --апи, которые возвращают результат
    API Select [User] -> encode Select.users
    API Select [Author] -> encode $ JSON.evalAuthors <$> Select.authors
    API Select [Category] -> encode Response.getCategories
    API Select [Tag] -> encode Select.tags
    API Select [Post] -> encode Response.getPosts
    API Select [Draft] -> encode Response.getDrafts
    API Select [Post, Id n, Comment] -> encode $ JSON.evalComments <$> Select.comments n

    API SelectById [User, Id n] -> encode $ Select.user n
    API SelectById [Author, Id n] -> encode $ (JSON.evalAuthor <$>) <$> Select.author n
    API SelectById [Category, Id n] -> encode $ Response.getCategory n
    API SelectById [Tag, Id n] -> encode $ Select.tag n
    API SelectById [Post, Id n] -> encode $ Response.getPost n
    API SelectById [Draft, Id n] -> encode $ Response.getDraft n

encode :: MT m => (Typeable a, ToJSON a) => m a -> m LC.ByteString
encode ta = do
    a <- ta
    json <- if showType a == "()" then encodePretty <$> Cache.getChanged else encodePretty <$> ta
    File.writeResponseJSON json
    return json

showType :: Typeable a => a -> String
showType = show . typeOf

getPosts :: MT m => m [JSON.Post]
getPosts = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- Response.getAllCategories
    Log.setSettings Color.Blue True "Response.getPosts" 
    Log.funcT Log.Debug "..."
    Cache.modifyParams $ JSON.evalParams categories
    selectPosts <- Select.posts
    JSON.evalPosts categories selectPosts

getDrafts :: MT m => m [JSON.Draft]
getDrafts = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- Response.getAllCategories
    Log.setSettings Color.Blue True "Response.getDrafts" 
    Log.funcT Log.Debug "..."
    Cache.modifyParams $ JSON.evalParams categories
    selectDrafts <- Select.drafts
    JSON.evalDrafts categories selectDrafts

getPost :: MT m => Int -> m (Maybe JSON.Post)
getPost pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- Response.getAllCategories
    Log.setSettings Color.Blue True "Response.getPost" 
    Log.funcT Log.Debug "..."
    selectPosts <- Select.post pid
    jsonPosts <- JSON.evalPosts categories selectPosts
    return $ listToMaybe jsonPosts --проверить как это работает. evalUnitedPosts должно объединять все в один пост

getDraft :: MT m => Int -> m (Maybe JSON.Draft)
getDraft pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- Response.getAllCategories
    Log.setSettings Color.Blue True "Response.getDraft" 
    Log.funcT Log.Debug "..."
    selectDrafts <- Select.draft pid
    jsonDrafts <- JSON.evalDrafts categories selectDrafts
    return $ listToMaybe jsonDrafts --проверить как это работает. evalUnitedDrafts должно объединять все в один пост

getAllCategories :: MT m => m [JSON.Category]
getAllCategories = do
    Log.setSettings Color.Blue True "Response.getAllCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    JSON.evalCategories allCategories allCategories

getCategories :: MT m => m [JSON.Category]
getCategories = do
    Log.setSettings Color.Blue True "Response.getCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    categories <- Select.categories
    JSON.evalCategories allCategories categories

updateCategory :: MT m => Int -> m () 
updateCategory pid = do 
    params <- Cache.getParams 
    Log.setSettings Color.Blue True "Response.updateCategory"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    JSON.checkCyclicCategory pid params allCategories
    Update.category pid

--эту логику перенести в select??
getCategory :: MT m => Int -> m (Maybe JSON.Category)
getCategory pid = do
    Log.setSettings Color.Blue True "Response.getCategory"
    Log.funcT Log.Debug "..."
    allCats <- Select.allCategories
    mcat <- Select.category pid
    sequenceA $ JSON.evalCategory allCats <$> mcat
