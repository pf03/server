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
import Select ( p, val )
import Data.Map as M ((!), fromList)
import qualified Data.Map as M (insert)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe
import API

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
author :: ParamsMap Param -> T Changed
author params = do 
    checkExist params "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --проверка на то, что данный автор уже существует
    checkNotExist "Автор" params "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    insert Author [sql|INSERT into authors (user_id, description)  values {0}|] [row params ["user_id", "description"]]   

--нельзя вставить категорию с нeсуществующим родителем, но можно вставить категорию без родителяб "parent_id" - необязаельный параметр
category :: ParamsMap Param -> T Changed
category params = do
    checkExist params "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    insert Category [sql|INSERT into categories (parent_id, category_name) values {0}|] [row params ["parent_id", "category_name"]]

tag :: ParamsMap Param -> T Changed
tag params = do
    checkNotExist "Тег" params "name" [sql|SELECT 1 FROM tags WHERE tags.name = {0}|]
    insert Tag [sql|INSERT into tags (name)  values ({0})|] [p $ params ! "name"]

--создать черновик и контент
--разделить варианты, когда черновик создается с нуля или к существующей новости!!!
--"author_id", "name", "creation_date", "category_id", "text", "photo", "news_id" - необязательный (ParamNo если создаем с нуля, ParamEq если к существующей новости)
--тут еще добавить теги и фотографии
draft :: Maybe Int -> ParamsMap Param -> T Changed
draft mpostId params = do
    when (params ! "author_id" == ParamEq (Int 1)) $ throwT $ DBError "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    postIdParam <- case mpostId of 
        Nothing -> return ParamNo
        Just postId -> return $ ParamEq (Int postId)
    checkExist params "author_id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
    checkExist params "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    [Only cid] <- query_ $ template [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|] 
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]] :: T[Only Int]
    insert Draft [sql|INSERT into drafts (content_id, post_id) values ({0}, {1})|] 
        [cell $ ParamEq (Int cid), cell postIdParam]

--опубликовать новость из черновика, черновик привязывется к новости, для дальнейшего редактирования
--"draft_id"
publish :: Int -> T Changed
publish pid = do
    let params = M.fromList [("draft_id", ParamEq (Int pid))] --костыль??
    checkExist params "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- query_ $ template [sql|SELECT content_id, post_id FROM drafts WHERE drafts.id = {0}|] 
        [p $ params ! "draft_id" ] :: T [(Int, Maybe Int)]
    case mpostId of 
        Nothing -> insert Post [sql|INSERT into posts (content_id) values ({0})|] [q contentId] --новость публикуется в первый раз
        Just postId -> update Post [sql|UPDATE posts SET content_id = {0} WHERE posts.id = {1}|] [q contentId, q postId]
    delete Draft [sql|DELETE FROM drafts WHERE drafts.id = {0}|] [p $ params ! "draft_id"]


    --checkExist params "content_id" [sql|SELECT 1 FROM contents WHERE contents.id = {0}|] мы его достали из базы, а не из параметров, поэтому проверять не надо
    --checkNotExist params "content_id" [sql|SELECT 1 FROM posts WHERE posts.contents_id = {0}|]  --проверка, что данная новость еще не опубликована
    
    

--UPDATE films SET kind = 'Dramatic' WHERE kind = 'Drama';

user :: ParamsMap Param -> T Changed
user params = do
    checkNotExist "Пользователь" params "login" [sql|SELECT 1 FROM users WHERE users.login = {0}|]
    insert User [sql|INSERT into users (last_name, first_name, avatar, login, pass, creation_date, is_admin) values {0}|]
        [rowEither params [Left "last_name", Left "first_name", Left "avatar", Left "login", Right $ template [sql|md5({0}), current_date, FALSE|] [cell(params ! "pass")]]]
    

comment :: Int -> ParamsMap Param -> T Changed
comment postId params = do
    when (params ! "user_id" == ParamEq (Int 1)) $ throwT $ DBError "Невозможно создать комментарий от удаленного пользователя (пользователя по умолчанию) id = 1"
    let allParams = M.insert "post_id" (ParamEq (Int postId)) params
    checkExist allParams "post_id" [sql|SELECT 1 FROM posts WHERE posts.id = {0}|]
    checkExist allParams "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    insert Comment [sql|INSERT into comments (post_id, user_id, creation_date, text) values {0}|]
        [rowEither allParams [Left "post_id", Left "user_id", Right [sql|current_date|], Left "text"]]
    

--query Select 1 ...
--в шаблон подставляется внутренний pid, если параметр обязательный, то ParamNo никогда не выскочит
checkExist :: ParamsMap Param -> BSName -> Query -> T() 
checkExist params name templ = helper name (params ! name) templ where
    helper name ParamNo templ = return ()
    helper name ParamNull templ = return ()
    helper name param@(ParamEq (Int pid)) templ = do 
        exist <- query_ $ template templ [q pid] :: T [Only Int]
        case exist of
            [] -> throwT $ DBError  (template "Указан несуществующий параметр {0}: {1}" [show name, show pid]) 
            _ -> return ()
            --x:xs -> ??
        --unless exist $ throwT $ DBError  (template "Уазан несуществующий parent_id: {0}" [show parentId]) 

checkNotExist :: String -> ParamsMap Param -> BSName -> Query -> T() 
checkNotExist description params name templ = helper name (params ! name) templ where
    helper name ParamNo templ = return ()
    helper name param@(ParamEq v) templ = do 
        exist <- query_ $ template templ [val v] :: T [Only Int]
        case exist of
            [] -> return ()
            _ -> throwT $ DBError  (template "{2} с таким {0} = {1} уже существует" [show name, toString v, description]) 
    toString :: Val -> String 
    toString (Int n) = show n
    toString (Str s) = show s
    toString (Date d) = show d



row :: ParamsMap Param -> [BSName] -> Query
row params names = list $ map (\name -> cell (params ! name)) names where
    -- helper :: BSName -> Query
    -- helper name = case params ! name of
    --     ParamEq v -> val v
    --     ParamNo -> [sql|null|]

--обобщенный ряд
rowEither :: ParamsMap Param -> [Either BSName Query] -> Query
rowEither params nqs = list $ map helper nqs where
    helper :: Either BSName Query -> Query
    helper nq = case nq of 
        Left name -> cell (params ! name)
        Right q -> q


cell :: Param -> Query
cell (ParamEq v) = val v
cell ParamNo = [sql|null|]



