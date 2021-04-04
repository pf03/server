{-# LANGUAGE FlexibleInstances #-}

module DB where
--import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import  qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Database.PostgreSQL.Simple.Time
import Class
import Types
import qualified Query 
import Query (q, (<+>), whereAll, inList)
-- import Data.Text
import Control.Monad.Except
import Control.Monad.Trans.Except
import Common
import Data.List
--import Database.PostgreSQL.Simple
import qualified Network.HTTP.Types.URI as HTTP
import Data.Maybe
import qualified Database.PostgreSQL.Simple.Types as SQL
import Data.Aeson hiding (encode)
import Control.Monad.Identity
import Transformer
import qualified Row
import qualified Select
import JSON

import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import NeatInterpolation
import qualified Data.Text.IO as T
import qualified System.Console.ANSI as Color
import Text.Read
import qualified Params
import qualified Insert
import API
import Router
import Data.Aeson.Encode.Pretty
import qualified Delete
import qualified State as S
import qualified Update
import qualified Network.Wai as Wai
import Network.Wai.Internal as Wai
import qualified Data.ByteString as B
import qualified Upload
import qualified Auth
import Network.HTTP.Types

import Data.Typeable

--эту обертку перенести в Response
getJSON_ :: Request -> T LC.ByteString
getJSON_ req = do
    S.resetChanged
    Log.setSettings Color.Blue True "DB.getJSONwithUpload" 
    Log.funcT Log.Debug "..."
    let (rawPathInfo, pathInfo, qs) = ( Wai.rawPathInfo req, Wai.pathInfo req, Wai.queryString req)

    requestBody <- toT $ streamOne (getRequestBodyChunk req)
    Log.debugT requestBody
    --эту функцию можно использовать для тестирования и эмуляции запросов
    let qsBody = parseQuery requestBody
    --Log.debugT req  
    logT $ Auth.auth req

    Auth.auth req
    auth <- S.getAuth

    api@(API apiType queryTypes) <- logT $ router rawPathInfo pathInfo auth
    
    params <- if apiType `elem` [API.Auth, API.Delete, API.Insert, API.Update] 
        then catchT (logT $ Params.parseParams qsBody api) $
            \(RequestError e) -> throwT $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в теле запроса методом x-www-form-urlencoded" [e] 
        else catchT (logT $ Params.parseParams qs api) $
            \(RequestError e) -> throwT $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в строке запроса" [e] 
    S.setParams params
    getJSON api req

--эта обертка для удобства тестирования--эту обертку перенести в Test
getJSONTest :: BC.ByteString -> PathInfo -> HTTP.Query -> HTTP.Query -> RequestHeaders -> T LC.ByteString
getJSONTest rawPathInfo pathInfo qs qsBody headers = do
    S.resetState --это нужно только для тестов, в сервере и так трансформер возрождается заново при каждом запросе
    --Log.setSettings Color.Blue True "DB.getJSONTest" 
    --Log.funcT Log.Debug "..."
    let req  = Wai.defaultRequest {requestHeaders = headers}
    Auth.auth req
    auth <- S.getAuth
    api@(API apiType queryTypes) <- logT $ router rawPathInfo pathInfo auth
    params <- if apiType `elem` [API.Auth, API.Delete, API.Insert, API.Update] 
        then catchT (logT $ Params.parseParams qsBody api) $
            \(RequestError e) -> throwT $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в теле запроса методом x-www-form-urlencoded" [e] 
        else catchT (logT $ Params.parseParams qs api) $
            \(RequestError e) -> throwT $ RequestError $ template "{0}.\n Внимание: параметры для данного запроса должны передаваться в строке запроса" [e] 
    S.setParams params
    getJSON api req

getJSON:: API -> Request -> T LC.ByteString
getJSON api req = case api of
    API Auth [] -> encode Auth.login

    API Upload [API.Photo] -> encode $ Upload.photo req

    --апи, которые не возвращают количество измененных строк
    --может publish сделать отдельным querytype?
    API Insert [API.User] -> encode Insert.user
    API Insert [API.Author] -> encode Insert.author
    API Insert [API.Category] -> encode Insert.category
    API Insert [API.Tag] -> encode Insert.tag
    API Insert [API.Draft] -> encode Insert.draft
    API Insert [API.Draft, Id n, API.Post] -> encode $ Insert.publish n
    API Insert [API.Post, Id n, API.Comment] -> encode $ Insert.comment n

    API Update [API.User, Id n] -> encode $ Update.user n
    API Update [API.Author, Id n] -> encode $ Update.author n
    API Update [API.Category, Id n] -> encode $ DB.updateCategory n
    API Update [API.Tag, Id n] -> encode $ Update.tag n
    API Update [API.Draft, Id n] -> encode $ Update.draft n
    API Update [API.Post, Id n] -> encode $ Update.post n

    API Delete [API.User, Id n] -> encode $ Delete.user n
    API Delete [API.Author, Id n] -> encode $ Delete.author n
    API Delete [API.Category, Id n] -> encode $ Delete.category n
    API Delete [API.Tag, Id n] -> encode $ Delete.tag n
    API Delete [API.Post, Id n] -> encode $ Delete.post n
    API Delete [API.Draft, Id n] -> encode $ Delete.draft n
    API Delete [API.Comment, Id n] -> encode $ Delete.comment n

    --апи, которые возвращают результат
    API Select [API.User] -> encode Select.users
    API Select [API.Author] -> encode $ evalAuthors <$> Select.authors
    API Select [API.Category] -> encode DB.getCategories
    API Select [API.Tag] -> encode Select.tags
    API Select [API.Post] -> encode DB.getPosts
    API Select [API.Draft] -> encode DB.getDrafts
    API Select [API.Post, Id n, API.Comment] -> encode $ evalComments <$> Select.comments n

    API SelectById [API.User, Id n] -> encode $ Select.user n
    API SelectById [API.Author, Id n] -> encode $ (evalAuthor <$>) <$> Select.author n
    API SelectById [API.Category, Id n] -> encode $ DB.getCategory n
    API SelectById [API.Tag, Id n] -> encode $ Select.tag n
    API SelectById [API.Post, Id n] -> encode $ DB.getPost n
    API SelectById [API.Draft, Id n] -> encode $ DB.getDraft n

encode :: (Typeable a, ToJSON a) => T a -> T LC.ByteString
encode ta = do
    a <- ta
    json <- if showType a == "()" then encodePretty <$> S.getChanged else encodePretty <$> ta
    writeResponseJSON json
    return json

showType :: Typeable a => a -> String
showType = show . typeOf

getPosts :: T [Post]
getPosts = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getPosts" 
    Log.funcT Log.Debug "..."
    S.modifyParams $ evalParams categories
    selectPosts <- Select.posts
    toT $ JSON.evalPosts categories selectPosts

getDrafts :: T [Draft]
getDrafts = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getDrafts" 
    Log.funcT Log.Debug "..."
    S.modifyParams $ evalParams categories
    selectDrafts <- Select.drafts
    toT $ JSON.evalDrafts categories selectDrafts

getPost :: Int -> T (Maybe Post)
getPost pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getPost" 
    Log.funcT Log.Debug "..."
    selectPosts <- Select.post pid
    jsonPosts <- toT $ JSON.evalPosts categories selectPosts
    return $ listToMaybe jsonPosts --проверить как это работает. evalUnitedPosts должно объединять все в один пост

getDraft :: Int -> T (Maybe Draft)
getDraft pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getDraft" 
    Log.funcT Log.Debug "..."
    selectDrafts <- Select.draft pid
    jsonDrafts <- toT $ JSON.evalDrafts categories selectDrafts
    return $ listToMaybe jsonDrafts --проверить как это работает. evalUnitedDrafts должно объединять все в один пост

getAllCategories :: T [Category]
getAllCategories = do
    Log.setSettings Color.Blue True "DB.getAllCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    toT $ evalCategories allCategories allCategories

getCategories :: T [Category]
getCategories = do
    Log.setSettings Color.Blue True "DB.getCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    categories <- Select.categories
    toT $ evalCategories allCategories categories

updateCategory :: Int -> T () 
updateCategory pid = do 
    params <- S.getParams 
    Log.setSettings Color.Blue True "DB.updateCategory"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    toT $ checkCyclicCategory pid params allCategories
    Update.category pid

--эту логику перенести в select??
getCategory :: Int -> T (Maybe Category)
getCategory pid = do
    Log.setSettings Color.Blue True "DB.getCategory"
    Log.funcT Log.Debug "..."
    allCats <- Select.allCategories
    mcat <- Select.category pid
    toT $ sequenceA $ evalCategory allCats <$> mcat