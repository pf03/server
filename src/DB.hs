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
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import NeatInterpolation
import qualified Data.Text.IO as T
import qualified System.Console.ANSI as Color
import Text.Read
import qualified Params

testq :: IO ()
testq = runT $ DB.getPosts Params.testQuery

--проверить некорректные запросы, например некорректный синтаксис запроса, или два раза и тот же параметр запроса
--в выходном json убрать префиксы с названиями таблиц бд
getUsers :: HTTP.Query -> T [User]
getUsers qs = do 
    Log.setSettings Color.Blue True "getUsers" 
    Log.funcT Log.Debug "..."
    params <- logT $ Params.parseParams qs "users"
    query <- logT $ Select.usersQuery params
    Query.query_ query

getAuthors :: HTTP.Query -> T [Author]
getAuthors qs = do
    Log.setSettings Color.Blue True "getAuthors"
    Log.funcT Log.Debug "..."
    params <- logT $ Params.parseParams qs "authors"
    query <- logT $ Select.authorsQuery params
    selected <- Query.query_ query
    return $ evalAuthors selected

getCategories :: HTTP.Query -> T [Category]
getCategories _ = getAllCategories

getAllCategories :: T [Category]
getAllCategories = do
    Log.setSettings Color.Blue False "getAllCategories"
    Log.funcT Log.Debug "..."
    query <- logT $ Select.categoriesQuery
    selected <- Query.query_ query
    logT $ evalCategories selected

getTags :: HTTP.Query -> T [Tag]
getTags qs = do
    Log.setSettings Color.Blue False "getTags" 
    Log.funcT Log.Debug "..."
    params <- logT $ Params.parseParams qs "tags"
    query <- logT $ Select.tagsQuery params
    Query.query_ query


-- нужно добавить также новости в дочерних категориях!!
--а также конкретизировать ошибки, например ошибка при парсинге page
--вывести на консоль query в корректной кодировке
--сделать Query экземпляром convert
getPosts :: HTTP.Query -> T [Post]
getPosts qs = do
    --эта строка первая, чтобы не перезаписывать настройки лога
    categories <- DB.getAllCategories
    Log.setSettings Color.Blue True "getPosts" 
    Log.funcT Log.Debug "..."
    params <- logT $ Params.parseParams qs "posts"

    -- ifJust textParam (putStrLnT . fromJust $ textParam)  --это выводит на консоль корректно, а запрос в бд некорректный
    -- ifJust textParam (putStrLnT . fromJust $ textParam1) --вообще виснет, а запрос в бд корректный

    --Если новость принадлежит к некоторой категории, то она принадлежит также и ко всем родительским категориям
    --ЗДЕСЬ ОБНОВИТЬ КАТЕГОРИЮ!!!
    let newParams = evalParams params categories

    --let query = Select.postsNewQuery pageParam tagParams categoryWithChildsParams textParam
    --Log.debugT query
    --это работает!!!!!!! подумать, как вывести на консоль
    query1 <- logT $ Select.postsNewQuery newParams
    selected <- Query.query_ query1
    json <- logT $ JSON.evalUnitedPosts categories selected
    writeResponse json
    return json



--многострочные запросы некорректно выводятся на консоль
--их нужно выводить куда-нибудь в файл