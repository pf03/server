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
import Query (q, (<+>))
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

testQuery :: HTTP.Query
testQuery = [("page", Just "1"), ("tag", Just "1")]

testq = showT $ DB.getPosts DB.testQuery

--проверить некорректные запросы, например некорректный синтаксис запроса, или два раза и тот же параметр запроса
getUsers :: HTTP.Query -> T [User]
getUsers qs = do 
    Log.debugT qs
    let selectQ = Select.userQuery
    let pageQ = pageQuery qs
    tagQ <- toT $ filterTagQuery qs
    let allQ = selectQ `Query.whereAll` [pageQ ,tagQ]
    Log.debugT allQ
    selectUsers <- Query.query_ allQ
    Log.debugT selectUsers
    return selectUsers

    -- let shoulbe = [sql|
    --     SELECT * FROM users 
    --         WHERE id BETWEEN 1 AND 20 
    --             AND tag = 1
    -- |]
    -- Log.debugT shoulbe
    -- Query.query_ shoulbe

getAuthors :: HTTP.Query -> T [Author]
getAuthors qs = do
    let selectQ = Select.authorQuery
    let pageQ = pageQuery qs
    tagQ <- toT $ filterTagQuery qs
    let allQ = selectQ `Query.whereAll` [pageQ ,tagQ]
    selectAuthors <- Query.query_ allQ
    return $ evalAuthors selectAuthors

getTags :: HTTP.Query -> T [Tag]
getTags queryString = do
    let selectQ = [sql|SELECT * FROM tags|]
    let pageQ = pageQuery queryString
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ
    tags <- Query.query_ allQ
    Log.debugT tags
    return tags 



getPosts :: HTTP.Query -> T [JSON.Post]
getPosts qs = do
    categories <- DB.getCategories qs

    let selectQ = Select.postQuery
    
    let pageQ = pageQuery qs
    --tagQ <- toT $ filterTagQuery queryString
    let allQ = selectQ `Query.whereAll` [pageQ]
    Log.debugT allQ


    selectPosts <- Query.query_ selectQ
    jsonPosts <- toT $ JSON.evalPosts categories selectPosts
    return jsonPosts
    --toT $ mapM (DB.evalPost categories) posts'




pageQuery :: HTTP.Query -> SQL.Query
pageQuery queryString = template [sql|id BETWEEN {0} AND {1}|] [q $ (page-1)*20+1, q $ page*20] where
        page = read . BC.unpack. fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ queryString :: Int


--tagList = ["tag", "tags__in", "tags__all"]

filterTagQuery :: HTTP.Query -> Except E SQL.Query 
filterTagQuery queryString = do
    mtagQuery <- lookupOne ["tag", "tags__in", "tags__all"] queryString
    case mtagQuery of 
        Nothing -> return mempty
        Just ("tag", tag) -> return $ template [sql|tag = {0}|] [q tag]
        Just ("tags__in", tag) -> throwE . DBError $ template "Обработка запроса {0} еще не реализована!" [show "tags__in"]

--в первом списке должно быть ровно одно значение из второго списка
lookupOne :: (Eq a, Show b, Show a) => [a] -> [(a, Maybe b)] -> Except E (Maybe (a, b)) 
lookupOne templates strs = do
    let results = map (\(a,b) -> (a, fromJust b)). filter (isJust . snd) . map (\t -> (t, lookup t strs)) $  templates
    case results of
        [] -> return Nothing 
        [(a, Nothing)] -> throwE . DBError $ template "Не указано значение параметра {0}" [show a]
        [(a, Just b)] -> return . Just $ (a, b)
        (r:rs) -> throwE . DBError $ template "В списке {0} должно быть не более одного значения из списка {1}" [show strs, show templates]



getCategories :: HTTP.Query -> T [Category]
getCategories qs = do 
    categories' <- Query.query_ [sql|SELECT * FROM categories|]
    toT $ JSON.evalCategories categories' 

-- getPosts_ :: HTTP.Query -> T [Post]
-- getPosts_ qs = do
--     categories <- DB.getCategories qs
--     posts' <- Query.query_ [sql|SELECT * FROM posts
--         LEFT JOIN contents ON contents.id = posts.content_id
--         LEFT JOIN authors ON authors.id = contents.author_id
--         LEFT JOIN users ON users.id = authors.user_id|]
--     toT $ JSON.evalPosts categories posts'


