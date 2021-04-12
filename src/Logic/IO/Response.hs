--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
--{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Logic.IO.Response where

-- Our Modules
import           Common.Misc
import           Interface.Cache            as Cache
import           Interface.DB               as DB
import           Interface.Error            as Error
import           Interface.Log              as Log
import qualified Logic.DB.Auth              as Auth
import qualified Logic.DB.Delete            as Delete
import qualified Logic.DB.Insert            as Insert
import qualified Logic.DB.Select            as Select
import qualified Logic.DB.Update            as Update
import qualified Logic.IO.File              as File
import qualified Logic.IO.Upload            as Upload
import qualified Logic.Pure.API             as API
import qualified Logic.Pure.JSON            as JSON
import qualified Logic.Pure.Params          as Params

-- Other Modules
import           Data.Aeson                 hiding (encode)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Network.HTTP.Types
import           Network.HTTP.Types.Header  (hContentType)
import qualified Network.Wai                as Wai
import           Network.Wai.Internal       as Wai
import qualified System.Console.ANSI        as Color
--import qualified Data.ByteString.Lazy as L
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List
import           Data.Maybe
import           Data.Typeable
import qualified Network.HTTP.Types.URI     as HTTP


-- можно сделать дополнительно MRequest для монады, которая имеет доступ к запросу, либо сделать ее часть Cache,
-- сто нелогично, так как Cache меняется, а Request нет. И request нужен не везде, где нужен Cache.
get :: MT m => Request -> m Response
get req = do
    Log.setSettings Color.Blue  True "Response.get"
    Log.dataT Log.Debug req
    json <- getJSON_ req
    Logic.IO.Response.json json

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

    api@(API apiType queryTypes) <- Log.logM $ API.router rawPathInfo pathInfo auth

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
    api@(API apiType queryTypes) <- Log.logM $ API.router rawPathInfo pathInfo auth
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

    --апи, которые возвращают результат
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
    categories <- getAllCategories
    Log.setSettings Color.Blue True "Response.getPosts"
    Log.funcT Log.Debug "..."
    Cache.modifyParams $ JSON.evalParams categories
    selectPosts <- Select.posts
    JSON.evalPosts categories selectPosts

getDrafts :: MT m => m [JSON.Draft]
getDrafts = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- getAllCategories
    Log.setSettings Color.Blue True "Response.getDrafts"
    Log.funcT Log.Debug "..."
    Cache.modifyParams $ JSON.evalParams categories
    selectDrafts <- Select.drafts
    JSON.evalDrafts categories selectDrafts

getPost :: MT m => Int -> m (Maybe JSON.Post)
getPost pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- getAllCategories
    Log.setSettings Color.Blue True "Response.getPost"
    Log.funcT Log.Debug "..."
    selectPosts <- Select.post pid
    jsonPosts <- JSON.evalPosts categories selectPosts
    return $ listToMaybe jsonPosts --проверить как это работает. evalUnitedPosts должно объединять все в один пост

getDraft :: MT m => Int -> m (Maybe JSON.Draft)
getDraft pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- getAllCategories
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
