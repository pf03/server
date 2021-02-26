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

testQuery :: HTTP.Query
--testQuery = [("page", Just "1"), ("tags__in", Just "[1,2,3]"),("category", Just "1")]
--testQuery = [("page", Just "1"), ("categories__in", Just "[1]")]
testQuery = [("page", Just "1"), ("tags__in", Just "[1,2,3]"),("category", Just "1"), ("text", Just "очередной")]

testq :: IO ()
testq = runT $ DB.getPosts DB.testQuery

--проверить некорректные запросы, например некорректный синтаксис запроса, или два раза и тот же параметр запроса
--в выходном json убрать префиксы с названиями таблиц бд
getUsers :: HTTP.Query -> T [User]
getUsers qs = do 
    Log.setSettings Color.Blue True "getUsers" 
    Log.funcT Log.Debug "..."
    pageParam <- logT $ getPageParam qs
    query <- logT $ Select.usersQuery pageParam
    Query.query_ query

getAuthors :: HTTP.Query -> T [Author]
getAuthors qs = do
    Log.setSettings Color.Blue True "getAuthors"
    Log.funcT Log.Debug "..."
    pageParam <- logT $ getPageParam qs
    query <- logT $ Select.authorsQuery pageParam
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
    pageParam <- logT $ getPageParam qs
    query <- logT $ Select.tagsQuery pageParam
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
    pageParam <- toT $ getPageParam qs
    tagParams <- toT $ getTagParams qs
    categoryParams <- toT $ getCategoryParams qs
    textParam1 <- toT $ getTextParam1 qs  

    -- ifJust textParam (putStrLnT . fromJust $ textParam)  --это выводит на консоль корректно, а запрос в бд некорректный
    -- ifJust textParam (putStrLnT . fromJust $ textParam1) --вообще виснет, а запрос в бд корректный

    --Если новость принадлежит к некоторой категории, то она принадлежит также и ко всем родительским категориям
    let categoryWithChildsParams = JSON.getChildCategories categoryParams categories

    --let query = Select.postsNewQuery pageParam tagParams categoryWithChildsParams textParam
    --Log.debugT query
    --это работает!!!!!!! подумать, как вывести на консоль
    query1 <- logT $ Select.postsNewQuery pageParam tagParams categoryWithChildsParams textParam1
    selected <- Query.query_ query1
    json <- toT $ JSON.evalUnitedPosts categories selected
    writeResponse json
    return json


--можно упростить и использовать eitherRead вместо eDecode!
getPageParam :: HTTP.Query -> Except E Int
getPageParam qs = do
        let quantity = 20
        page <- eDecode . convertL . fromMaybe "1" . fromMaybe (Just "1") . lookup "page" $ qs
        return page

getTagParams :: HTTP.Query -> Except E (Params Int)
getTagParams qs = do
    mtagQuery <- lookupOne ["tag", "tags__in", "tags__all"] qs
    case mtagQuery of 
        Nothing -> return ParamsAny
        Just ("tag", value) -> do
            tagInt <- eDecode . convertL $ value
            return $ ParamsIn [tagInt]
        Just ("tags__in", list) -> do
            tagsList <- eDecode . convertL $ list
            return $ ParamsIn tagsList
        Just ("tags__all", list) -> do
            tagsList <- eDecode . convertL $ list
            return $ ParamsAll tagsList

--unpack нужен, чтобы потом сделать encodeUtf8 для кириллицы
--это выводит на консоль корректно, а запрос в бд некорректный
-- getTextParam :: HTTP.Query -> Maybe String
-- getTextParam =  fmap (BC.unpack) . fromMaybe Nothing . lookup "text"


--вообще виснет при выводе на консоль, а запрос в бд корректный
getTextParam1 :: HTTP.Query -> Identity (Maybe String)
getTextParam1 = return . fmap ( T.unpack . T.decodeUtf8 ) . fromMaybe Nothing . lookup "text"


--универсализировать, выкинуть ошибку при categories__all
getCategoryParams :: HTTP.Query -> Except E (Params Int)
getCategoryParams qs = do
    mtagQuery <- lookupOne ["category", "categories__in"] qs
    case mtagQuery of 
        Nothing -> return ParamsAny
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