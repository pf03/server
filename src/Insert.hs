{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Insert where

import Database.PostgreSQL.Simple.FromRow --hiding (FromRow(..) ) 
import Database.PostgreSQL.Simple.Time
import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, Text(..))
import Types 
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common
import Query
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Identity
import Select
import Data.Map as M ((!), fromList)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe

-- Должно быть отдельные API для сущностей:
-- авторов — и создание, и редактирование, и получение, и удаление, только для админов, 
-- категории — получение всем, создание, удаление и редактирование только для админов, 
-- теги — получение всем, создание, удаление и редактирование только для админов, 
-- черновики — создание, редактирование, получение, удаление всем авторам только своих черновиков, плюс отдельный метод publish, чтобы апдейтнуть публикацию
-- пользователей  — создание, получение всем (редактирования нет), удаление только админам
-- В "получение" входит перечисление всех пользователей на сайте? Или нужно только выдавать информацию о конкретном пользователе по его заранее известному айди?, или

-- Хороший вопрос, забыли это явно указать: получить можно только своего юзера и указывать для этого айди не надо, достаточно того, что по сессии будет понятно, что это за юзер. То есть можно получить по API инфу по своему юзеру и всё, а указывать свой айди для этого даже не надо

-- комментарии — создание, получение списка комментариев для определенного поста 
-- Урл: /posts/123/comments
-- , удаление. Редактирование и получение отдельного комментария необязательны.

-- tag :: ParamsMap Param ->  Identity Query
-- tag params = return res where
--     ParamEq v = params ! "name"
--     res = template [sql|INSERT into tags (name) values ({0})|] [val v]
--"user_id" - обязаельный параметр
author :: ParamsMap Param -> T()
author params = do 
    checkExist params "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --проверка на то, что данный автор уже существует
    checkNotExist "Автор" params "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    execute__ $ template [sql|INSERT into authors (user_id, description)  values {0}|] [row params ["user_id", "description"]]   

--нельзя вставить категорию с нeсуществующим родителем, но можно вставить категорию без родителяб "parent_id" - необязаельный параметр
category :: ParamsMap Param -> T()
category params = do
    checkExist params "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    execute__ $ template [sql|INSERT into categories (parent_id, category_name) values {0}|] [row params ["parent_id", "category_name"]]

tag :: ParamsMap Param -> T ()
tag params = execute__ $ template [sql|INSERT into tags (name)  values ({0})|] [p $ params ! "name"]

--создать черновик и контент
--разделить варианты, когда черновик создается с нуля или к существующей новости!!!
--"author_id", "name", "creation_date", "category_id", "text", "photo", "news_id" - необязательный (ParamNo если создаем с нуля, ParamEq если к существующей новости)
--тут еще добавить теги и фотографии
draft :: ParamsMap Param -> T ()
draft params = do
    checkExist params "author_id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
    checkExist params "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    [Only cid] <- query_ $ template [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|] 
        [row params ["author_id", "name", "creation_date", "category_id", "text", "photo"]] :: T[Only Int]
    execute__ $ template [sql|INSERT into drafts (content_id, news_id) values ({0}, {1})|] [cell $ ParamEq (Int cid), cell (params!"news_id")]

--опубликовать новость из черновика, черновик привязывется к новости, для дальнейшего редактирования
--"draft_id"
publish :: Int -> T()
publish pid = do
    let params = M.fromList [("draft_id", ParamEq (Int pid))] --костыль??
    checkExist params "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- query_ $ template [sql|SELECT (content_id, posts_id) FROM drafts WHERE drafts.id = {0}|] 
        [p $ params ! "draft_id" ] :: T [(Int, Maybe Int)]
    case mpostId of 
        Nothing -> execute__ $ template [sql|INSERT into posts (content_id) values ({0})|] [q contentId] --новость публикуется в первый раз
        Just postId -> execute__ $ template [sql|UPDATE posts SET content_id = {0} WHERE posts.id = {1}|] [q contentId, q postId]
    execute__ $ template [sql|DELETE FROM drafts WHERE drafts.id = {0}|] [p $ params ! "draft_id"]


    --checkExist params "content_id" [sql|SELECT 1 FROM contents WHERE contents.id = {0}|] мы его достали из базы, а не из параметров, поэтому проверять не надо
    --checkNotExist params "content_id" [sql|SELECT 1 FROM posts WHERE posts.contents_id = {0}|]  --проверка, что данная новость еще не опубликована
    
    

--UPDATE films SET kind = 'Dramatic' WHERE kind = 'Drama';

user :: ParamsMap Param -> T()
user params = execute__ $ template [sql|INSERT into users (last_name, first_name, avatar, login, pass, creation_date, is_admin) values {0}|]
        [row params ["last_name", "first_name", "avatar", "login", "pass", "creation_date", "is_admin"]]
    

comment :: Int -> ParamsMap Param -> T()
comment =  undefined
    

--query Select 1 ...
--в шаблон подставляется внутренний pid, если параметр обязательный, то ParamNo никогда не выскочит
checkExist :: ParamsMap Param -> BSName -> Query -> T() 
checkExist params name templ = helper name (params ! name) templ where
    helper name ParamNo templ = return ()
    helper name param@(ParamEq (Int pid)) templ = do 
        exist <- query_ $ template templ [q pid] :: T [Only Int]
        case exist of
            [] -> throwT $ RequestError  (template "Уазан несуществующий параметр {0}: {1}" [show name, show pid]) 
            _ -> return ()
            --x:xs -> ??
        --unless exist $ throwT $ RequestError  (template "Уазан несуществующий parent_id: {0}" [show parentId]) 

checkNotExist :: String -> ParamsMap Param -> BSName -> Query -> T() 
checkNotExist description params name templ = helper name (params ! name) templ where
    helper name ParamNo templ = return ()
    helper name param@(ParamEq (Int pid)) templ = do 
        exist <- query_ $ template templ [q pid] :: T [Only Int]
        case exist of
            [] -> return ()
            _ -> throwT $ RequestError  (template "{2} с таким {0} = {1} уже существует" [show name, show pid, description]) 
            



row :: ParamsMap Param -> [BSName] -> Query
row params names = list $ map (\name -> cell (params ! name)) names where
    -- helper :: BSName -> Query
    -- helper name = case params ! name of
    --     ParamEq v -> val v
    --     ParamNo -> [sql|null|]

cell :: Param -> Query
cell (ParamEq v) = val v
cell ParamNo = [sql|null|]

