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

-- testq :: IO ()
-- testq = runT $ DB.insertAuthor DB.testQueryInsert

-- testQueryInsert :: HTTP.Query
-- testQueryInsert = [
--         ("user_id", Just "666"),
--         ("description", Just "Unknown author"),

--         --("parent_id", Just "1"),
--         ("category_name", Just "Unknown category")
--     ]





--ВМЕСТО ВСЕГО ЭТОГО БЕЗОБРАЗИЯ МОЖНО СДЕЛАТЬ УНИВЕРСАЛЬНУЮ ФУНКЦИЮ, НО УЧЕСТЬ, ЧТО ОНА НЕ МОЖЕТ ВОЗВРАЩАТЬ РЕЗУЛЬТАТ РАЗНЫХ  ТИПОВ.
--ТАК ЧТО ОНА БУДЕТ ВОЗВРАЩАТЬ СРАЗУ json!!!
--ИЛИ НЕКОЛЬКО ФУНКЦИЙ НА РАЗНЫЕ ТИПЫ ЗАПРОСОВ

--проверить некорректные запросы, например некорректный синтаксис запроса, или два раза и тот же параметр запроса
--в выходном json убрать префиксы с названиями таблиц бд
-- getUsers :: HTTP.Query -> T [User]
-- getUsers qs = do 
--     Log.setSettings Color.Blue True "getUsers" 
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Select API.User 
--     query <- logT $ Select.usersQuery params
--     Query.query_ query

-- getAuthors :: HTTP.Query -> T [Author]
-- getAuthors qs = do
--     Log.setSettings Color.Blue True "getAuthors"
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Select API.Author 
--     query <- logT $ Select.authorsQuery params
--     selected <- Query.query_ query
--     return $ evalAuthors selected

-- getCategories :: HTTP.Query -> T [Category]
-- getCategories _ = getAllCategories



-- getTags :: HTTP.Query -> T [Tag]
-- getTags qs = do
--     Log.setSettings Color.Blue False "getTags" 
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Select API.Tag 
--     query <- logT $ Select.tagsQuery params
--     Query.query_ query


-- -- нужно добавить также новости в дочерних категориях!!
-- --а также конкретизировать ошибки, например ошибка при парсинге page
-- --вывести на консоль query в корректной кодировке
-- --сделать Query экземпляром convert

-- getPosts :: HTTP.Query -> T [Post]
-- getPosts qs = do
--     --эта строка первая, чтобы не перезаписывать настройки лога
--     categories <- DB.getAllCategories
--     Log.setSettings Color.Blue True "getPosts" 
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Select API.Post 

--     -- ifJust textParam (putStrLnT . fromJust $ textParam)  --это выводит на консоль корректно, а запрос в бд некорректный
--     -- ifJust textParam (putStrLnT . fromJust $ textParam1) --вообще виснет, а запрос в бд корректный

--     --Если новость принадлежит к некоторой категории, то она принадлежит также и ко всем родительским категориям
--     --ЗДЕСЬ ОБНОВИТЬ КАТЕГОРИЮ!!!
--     let newParams = evalParams params categories

--     --let query = Select.postsNewQuery pageParam tagParams categoryWithChildsParams textParam
--     --Log.debugT query
--     --это работает!!!!!!! подумать, как вывести на консоль
--     query1 <- logT $ Select.postsNewQuery newParams
--     selected <- Query.query_ query1
--     json <- logT $ JSON.evalUnitedPosts categories selected
--     writeResponse json
--     return json



-- --многострочные запросы некорректно выводятся на консоль
-- --их нужно выводить куда-нибудь в файл

-- insertTag :: HTTP.Query -> T()
-- insertTag qs = do
--     Log.setSettings Color.Blue True "insertTag" 
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Insert API.Tag 
--     Insert.tag params
--     -- Query.execute__ query

-- insertAuthor :: HTTP.Query -> T()
-- insertAuthor qs = do
--     Log.setSettings Color.Blue True "insertAuthor" 
--     Log.funcT Log.Debug "..."
--     params <- toT $ Params.parseParams qs $ API API.Insert API.Author 
--     Insert.author params
--     -- Query.execute__ query

-- insertCategory :: HTTP.Query -> T()
-- insertCategory qs = do
--     Log.setSettings Color.Blue True "insertCategory" 
--     Log.funcT Log.Debug "..."
--     params <- logT $ Params.parseParams qs $ API API.Insert API.Category 
--     Insert.category params
--     -- Query.execute__ query


-- editTag :: HTTP.Query -> T()
-- editTag = undefined

-- deleteTag :: HTTP.Query -> T()
-- deleteTag = undefined

--общая функция
getJSON:: BC.ByteString -> PathInfo -> HTTP.Query -> T LC.ByteString
getJSON rawPathinfo pathInfo qs = do



    
    Log.setSettings Color.Blue True "DB.getJSON" 
    Log.funcT Log.Debug "..."
    Log.debugT qs
    S.resetChanged
    api@(API apiType queryTypes) <- logT $ router rawPathinfo pathInfo
    params <- logT $ Params.parseParams qs api
    
    case api of
        --апи, которые не возвращают количество измененных строк
        API Insert [API.User] -> encode $ Insert.user params
        API Insert [API.Author] -> encode $ Insert.author params
        API Insert [API.Category] -> encode $ Insert.category params
        API Insert [API.Tag] -> encode $ Insert.tag params
        API Insert [API.Draft] -> encode $ Insert.draft params
        API Insert [API.Draft, Id n, API.Post] -> encode $ Insert.publish n
        API Insert [API.Post, Id n, API.Comment] -> encode $ Insert.comment n params

        API Delete [API.User, Id n] -> encode $ Delete.user n
        API Delete [API.Author, Id n] -> encode $ Delete.author n
        API Delete [API.Post, Id n] -> encode $ Delete.post n
        API Delete [API.Comment, Id n] -> encode $ Delete.comment n

        API Update [API.User, Id n] -> encode $ Update.user n params
        API Update [API.Author, Id n] -> encode $ Update.author n params
        API Update [API.Category, Id n] -> encode $ DB.updateCategory n params
        API Update [API.Tag, Id n] -> encode $ Update.tag n params
        API Update [API.Draft, Id n] -> encode $ Update.draft n params

        --апи, которые возвращают результат
        API SelectById [API.User, Id n] -> encode $ Select.user n
        API SelectById [API.Author, Id n] -> encode $ (evalAuthor <$>) <$> Select.author n
        API SelectById [API.Category, Id n] -> encode $ DB.getCategory n
        API SelectById [API.Post, Id n] -> encode $ DB.getPost n
        API SelectById [API.Tag, Id n] -> encode $ Select.tag n

        API Select [API.User] -> encode $ Select.users params
        API Select [API.Author] -> encode $ evalAuthors <$> Select.authors params
        API Select [API.Category] -> encode $ DB.getCategories params
        API Select [API.Post] -> encode $ DB.getPosts params
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
    jsonPosts <- toT $ JSON.evalUnitedPosts categories selectPosts
    return jsonPosts

getPost :: Int -> T (Maybe Post)
getPost pid = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "DB.getPost" 
    Log.funcT Log.Debug "..."
    selectPosts <- Select.post pid
    jsonPosts <- logT $ JSON.evalUnitedPosts categories selectPosts
    return $ listToMaybe jsonPosts --проверить как это работает. evalUnitedPosts должно объединять все в один пост

getAllCategories :: T [Category]
getAllCategories = do
    Log.setSettings Color.Blue True "DB.getAllCategories"
    Log.funcT Log.Debug "..."
    allCategories <- Select.allCategories
    logT $ evalCategories allCategories allCategories

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

--функция, которая не возвращает json (insert,update,delete) 
-- execute :: BC.ByteString -> PathInfo -> HTTP.Query -> T()
-- execute rawPathinfo pathInfo qs = do
--     Log.setSettings Color.Blue True "DB.execute" 
--     Log.funcT Log.Debug "..."
--     api <- logT $ router rawPathinfo pathInfo
--     params <- logT $ Params.parseParams qs api
--     case api of
--         API Insert [API.User] -> Insert.user params
--         API Insert [API.Author] -> Insert.author params
--         API Insert [API.Category] -> Insert.category params
--         API Insert [API.Tag] -> Insert.tag params
--         API Insert [API.Draft] -> Insert.draft params
--         API Insert [API.Draft, Id pid, API.Post] -> Insert.publish pid  
--         API Insert [API.Post, Id pid, API.Comment] -> Insert.comment pid params



-- router _ ["users", "create"] = return $ API Insert [User]
-- router _ ["authors", "create"] = return $ API Insert [Author]
-- router _ ["categories", "create"] = return $ API Insert [Category]
-- router _ ["tags", "create"] = return $ API Insert [Tag]
-- router _ ["drafts", "create"] = return $ API Insert [Draft]
-- router _ ["drafts", n, "publish"] = withInt "post_id" n $ \pid -> API Insert [Draft, Id pid, Post] --метод drafts publish заменяет posts create и posts edit и подчеркивает, что новость нельзя опубликовать напрямую без черновика (премодерации)
-- router _ ["posts", n, "comments", "create"] = withInt "post_id" n $ \pid -> API Insert [Post, Id pid, Comment]