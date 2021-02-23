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
import Data.Aeson
import Control.Monad.Identity
import Transformer
import qualified Row
import qualified Select
import JSON
import Database.PostgreSQL.Simple
import Error 
import Parse

testQuery :: HTTP.Query
testQuery = [("page", Just "foo"), ("tags__in", Just "[1,2,3]")]

testq = runT $ DB.getPosts DB.testQuery

--проверить некорректные запросы, например некорректный синтаксис запроса, или два раза и тот же параметр запроса
--в выходном json убрать префиксы с названиями таблиц бд
getUsers :: HTTP.Query -> T [User]
getUsers qs = do 
    let tname = "users"
    Log.debugT qs
    let selectQ = Select.usersQuery
    let pageQ = pageQuery qs tname
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ
    select <- Query.query_ allQ
    --Log.debugT select
    return select

    -- let shoulbe = [sql|
    --     SELECT * FROM users 
    --         WHERE id BETWEEN 1 AND 20 
    --             AND tag = 1
    -- |]
    -- Log.debugT shoulbe
    -- Query.query_ shoulbe

getAuthors :: HTTP.Query -> T [Author]
getAuthors qs = do
    let tname = "authors"
    let selectQ = Select.authorsQuery
    let pageQ = pageQuery qs tname
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ
    select <- Query.query_ allQ
    Log.debugT select
    return $ evalAuthors select

getCategories :: HTTP.Query -> T [Category]
getCategories qs = do 
    let tname = "categories"
    let selectQ = Select.categoriesQuery
    let pageQ = pageQuery qs tname
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ
    select <- Query.query_ allQ
    Log.debugT select
    toT $ evalCategories select

getAllCategories :: T [Category]
getAllCategories = do
    let tname = "categories"
    let selectQ = Select.categoriesQuery
    Log.debugT selectQ
    select <- Query.query_ selectQ
    Log.debugT select
    toT $ evalCategories select



getTags :: HTTP.Query -> T [Tag]
getTags qs = do
    let tname ="tags"
    let selectQ = Select.tagsQuery
    let pageQ = pageQuery qs tname
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ
    tags <- Query.query_ allQ
    Log.debugT tags
    return tags 



getPosts :: HTTP.Query -> T [Post]
getPosts qs = do
    categories <- DB.getAllCategories
    tagParams <- toT $ getTagParams qs
    let pageParam = getPageParam qs
    let allQ = Select.postsNewQuery pageParam tagParams
    Log.debugT allQ
    selected <- Query.query_ allQ
    Log.debugT selected
    json <- toT $ JSON.evalUnitedPosts categories selected
    Log.debugT json
    writeResponse json
    return json





--как-то повысить читаемость кода
--сделать отдельную функцию для формирования запроса
-- getPosts :: HTTP.Query -> T [Post]
-- getPosts qs = do
--     let tname ="posts"
--     categories <- DB.getAllCategories
--     let pageQ = pageQuery qs tname

--     tagQ <- toT $ filterTagQuery qs
--     let postIdsQ = Select.postIdsQuery `Query.whereAll` [tagQ] <+> pageQ
--     Log.debugT postIdsQ
--     postOnlyIds <- Query.query_ postIdsQ  ::T [Only Int]
--     let postIds = map fromOnly postOnlyIds
--     Log.debugT postIds
--     let allQ = Select.postsQuery `whereAll` ["posts.id"  `inList` postIds]
--     Log.debugT allQ
--     selectPosts <- Query.query_ allQ
--     jsonPosts <- toT $ JSON.evalUnitedPosts categories selectPosts
--     writeResponse jsonPosts --убрать после отладки
--     postIds2 <- Query.query_ Select.exist::T [Only Int]
--     Log.debugT postIds2
--     return jsonPosts

--debug wrapper
-- wrapper :: (FromRow a, Show a, ToJSON a) => SQL.Query -> T [a]
-- wrapper query = do
--     Log.debugT query
--     select <- Query.query_ query
--     writeResponse select
--     return select


--здесь нужно более хитрое ограничение на количество возвращаемых строк c уникальным id, а не просто по id!!!
-- pageQuery :: HTTP.Query -> String -> SQL.Query
-- pageQuery qs tname = template [sql|{2}.id BETWEEN {0} AND {1}|] [q $ (page-1)*quantity+1, q $ page*quantity, q tname] where
--         page = read . BC.unpack. fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs :: Int
--         quantity = 20

pageQuery :: HTTP.Query -> String -> SQL.Query
pageQuery qs _ = template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
        page = read . BC.unpack. fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs :: Int
        quantity = 20

--рассмотреть вариант ошибки, типа page="foo"
getPageParam :: HTTP.Query -> Int
getPageParam qs = page where
        page = read . BC.unpack. fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs :: Int
        quantity = 20


--tagList = ["tag", "tags__in", "tags__all"]


--всегда нужно проверять тип значения в queryString!
filterTagQuery :: HTTP.Query -> Except E SQL.Query 
filterTagQuery queryString = do
    mtagQuery <- lookupOne ["tag", "tags__in", "tags__all"] queryString
    case mtagQuery of 
        Nothing -> return mempty
        Just ("tag", tag) -> do
            tagInt <- eDecode . convertL $ tag :: Except E Int
            return $ template [sql|tags.id = {0}|] [q tagInt]
        Just ("tags__in", tag) -> do
            tagsList <- eDecode . convertL $ tag :: Except E [Int]
            return $ "tags.id" `inList` tagsList 
        Just ("tags__all", tag) -> throwE . DBError $ template "Обработка запроса {0} еще не реализована!" [show "tags__all"]

getTagParams :: HTTP.Query -> Except E (Params Int)
getTagParams qs = do
    mtagQuery <- lookupOne ["tag", "tags__in", "tags__all"] qs
    case mtagQuery of 
        Nothing -> return ParamsEmpty
        Just ("tag", value) -> do
            tagInt <- eDecode . convertL $ value
            return $ ParamsIn [tagInt]
        Just ("tags__in", list) -> do
            tagsList <- eDecode . convertL $ list
            return $ ParamsIn tagsList
        Just ("tags__all", list) -> do
            tagsList <- eDecode . convertL $ list
            return $ ParamsAll tagsList


--универсализировать
getCategoryParams :: HTTP.Query -> Except E (Params Int)
getCategoryParams qs = do
    mtagQuery <- lookupOne ["category", "categories__in"] qs
    case mtagQuery of 
        Nothing -> return ParamsEmpty
        Just ("category", value) -> do
            valueInt <- eDecode . convertL $ value
            return $ ParamsIn [valueInt]
        Just ("categories__in", list) -> do
            valuesList <- eDecode . convertL $ list
            return $ ParamsIn valuesList

--в первом списке должно быть ровно одно значение из второго списка
lookupOne :: (Eq a, Show b, Show a) => [a] -> [(a, Maybe b)] -> Except E (Maybe (a, b)) 
lookupOne templates strs = do
    let results = map (\(a,b) -> (a, fromJust b)). filter (isJust . snd) . map (\t -> (t, lookup t strs)) $  templates
    case results of
        [] -> return Nothing 
        [(a, Nothing)] -> throwE . DBError $ template "Не указано значение параметра {0}" [show a]
        [(a, Just b)] -> return . Just $ (a, b)
        (r:rs) -> throwE . DBError $ template "В списке {0} должно быть не более одного значения из списка {1}" [show strs, show templates]

--многострочные запросы некорректно выводятся на консоль
--их нужно выводить куда-нибудь в файл