{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module DB where
import Database.PostgreSQL.Simple.FromRow
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
import Database.PostgreSQL.Simple
import Error 
import Parse
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

--это обертка для загрузки файлов, подумать о более красивом реешении
getJSONwithUpload :: Request -> T LC.ByteString
getJSONwithUpload req = do
    let (rawPathInfo, pathInfo, qs) = ( Wai.rawPathInfo req, Wai.pathInfo req, Wai.queryString req)
    getJSON rawPathInfo pathInfo qs req

--эта обертка для удобства тестирования, req нужен для upload
getJSON:: BC.ByteString -> PathInfo -> HTTP.Query -> Request -> T LC.ByteString
getJSON rawPathInfo pathInfo qs req = do

    Log.setSettings Color.Blue True "DB.getJSON" 
    Log.funcT Log.Debug "..."
    
    Log.debugT qs
    S.resetChanged
    api@(API apiType queryTypes) <- logT $ router rawPathInfo pathInfo
    params <- logT $ Params.parseParams qs api
    
    case api of
        API Upload [API.Photo] -> encode $ Upload.photo req params

        --апи, которые не возвращают количество измененных строк
        API Insert [API.User] -> encode $ Insert.user params
        API Insert [API.Author] -> encode $ Insert.author params
        API Insert [API.Category] -> encode $ Insert.category params
        API Insert [API.Tag] -> encode $ Insert.tag params
        API Insert [API.Draft] -> encode $ Insert.draft Nothing params
        API Insert [API.Draft, Id n, API.Post] -> encode $ Insert.publish n
        API Insert [API.Post, Id n, API.Comment] -> encode $ Insert.comment n params

        API Update [API.User, Id n] -> encode $ Update.user n params
        API Update [API.Author, Id n] -> encode $ Update.author n params
        API Update [API.Category, Id n] -> encode $ DB.updateCategory n params
        API Update [API.Tag, Id n] -> encode $ Update.tag n params
        API Update [API.Draft, Id n] -> encode $ Update.draft n params
        API Update [API.Post, Id n] -> encode $ Insert.draft (Just n) params

        API Delete [API.User, Id n] -> encode $ Delete.user n
        API Delete [API.Author, Id n] -> encode $ Delete.author n
        API Delete [API.Post, Id n] -> encode $ Delete.post n
        API Delete [API.Draft, Id n] -> encode $ Delete.draft n
        API Delete [API.Comment, Id n] -> encode $ Delete.comment n

        --апи, которые возвращают результат
        API SelectById [API.User, Id n] -> encode $ Select.user n
        API SelectById [API.Author, Id n] -> encode $ (evalAuthor <$>) <$> Select.author n
        API SelectById [API.Category, Id n] -> encode $ DB.getCategory n
        API SelectById [API.Post, Id n] -> encode $ DB.getPost n
        API SelectById [API.Draft, Id n] -> encode $ DB.getDraft n
        API SelectById [API.Tag, Id n] -> encode $ Select.tag n

        API Select [API.User] -> encode $ Select.users params
        API Select [API.Author] -> encode $ evalAuthors <$> Select.authors params
        API Select [API.Category] -> encode $ DB.getCategories params
        API Select [API.Post] -> encode $ DB.getPosts params
        API Select [API.Draft] -> encode $ DB.getDrafts params
        API Select [API.Tag] -> encode $ Select.tags params
        API Select [API.Post, Id n, API.Comment] -> encode $ evalComments <$> Select.comments n params

encode :: ToJSON a => T a -> T LC.ByteString
encode ta = do
    a <- ta
    let json = encodePretty a
    writeResponseJSON json
    return json

getPosts :: ParamsMap Param -> T [Post]
getPosts params = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getPosts" 
    Log.funcT Log.Debug "..."
    let newParams = evalParams params categories
    selectPosts <- Select.posts newParams
    jsonPosts <- toT $ JSON.evalPosts categories selectPosts
    return jsonPosts

getDrafts :: ParamsMap Param -> T [Draft]
getDrafts params = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getDrafts" 
    Log.funcT Log.Debug "..."
    let newParams = evalParams params categories
    selectDrafts <- Select.drafts newParams
    jsonDrafts <- toT $ JSON.evalDrafts categories selectDrafts
    return jsonDrafts

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

getCategories :: ParamsMap Param ->  T [Category]
getCategories params = do
    Log.setSettings Color.Blue True "DB.getCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    categories <- Select.categories params
    toT $ evalCategories allCategories categories

updateCategory :: Int -> ParamsMap Param -> T Changed 
updateCategory pid params = do 
    Log.setSettings Color.Blue True "DB.updateCategory"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    toT $ checkCyclicCategory pid params allCategories
    Update.category pid params

--эту логику перенести в select??
getCategory :: Int -> T (Maybe Category)
getCategory pid = do
    Log.setSettings Color.Blue True "DB.getCategory"
    Log.funcT Log.Debug "..."
    allCats <- Select.allCategories
    mcat <- Select.category pid
    toT $ sequenceA $ evalCategory allCats <$> mcat