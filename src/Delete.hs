{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Delete where

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
import Data.Map as M ((!), fromList)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe
import Data.Monoid
import Data.Int
import API
import qualified State as S
import Select
import Update

-- Удаление 4 типов
-- 1. Удаленная сущность заменяется на значение по умолчанию. Используется для users и authors
-- 2. Каскадное удаление вместе с привязанными сущностями. Используется для posts и drafts
-- 3. Удаление строго по условию, если к данной сущности ничего не привязано. Сначала нужно отредактировать или удалить 
--связанные сущности, а потом продолжить удаление. Используется для categories.
-- 4. Простое удаление, если от сущности ничего не зависит. Используется для comments

--юзер удаляется, автор привязывается к дефолтному юзеру, авторизация админ на уровне роутера
user :: Int -> T ()
user pid = do 
    when (pid == 1) $ throwT $ DBError "Невозможно удалить пользователя по умолчанию с id = 1"
    when (pid == 2) $ throwT $ DBError "Невозможно удалить админа с id = 2"
    update Author [sql|UPDATE authors SET user_id = 1 WHERE user_id = {0}|] [q pid]
    update Comment [sql|UPDATE comments SET user_id = 1 WHERE user_id = {0}|] [q pid]
    delete User [sql|DELETE FROM users WHERE id = {0}|] [q pid]

--юзер удаляется, автор привязывается к дефолтному юзеру, авторизация админ на уровне роутера
author :: Int -> T ()
author pid = do 
    when (pid == 1) $ throwT $ DBError "Невозможно удалить автора по умолчанию с id = 1"
    update Content [sql|UPDATE contents SET author_id = 1 WHERE author_id = {0}|] [q pid] 
    delete Author [sql|DELETE FROM authors WHERE id = {0}|] [q pid]

--для поста каскадное удаление
post :: Int -> T () 
post pid = do 
    (_, _, contentId) <- checkAuthExistPost pid
    delete Post [sql|DELETE FROM posts WHERE id = {0}|] [q pid]   
    delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q contentId]   
    delete Draft [sql|DELETE FROM drafts WHERE post_id = {0}|] [q pid] 
    execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q contentId]   
    delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q contentId]   
    delete Comment [sql|DELETE FROM comments WHERE post_id = {0}|] [q pid]

--для черновика каскадное удаление
draft :: Int -> T ()
draft pid = do
    (_, _, contentId) <- checkAuthExistDraft pid
    delete Draft [sql|DELETE FROM drafts WHERE id = {0}|] [q pid]   
    delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q contentId]   
    execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q contentId]   
    delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q contentId]

comment :: Int -> T ()
comment pid = do 
    checkAuthExistComment pid
    delete Comment [sql|DELETE FROM comments WHERE id = {0}|] [q pid] 

--удаление строго по условию, если не привязаны другие категории и контент
category :: Int -> T ()
category pid = do
    --проверка на связанные сущности
    checkNotExist pid "категорию" "дочерние категории" $ template [sql|
        SELECT id, category_name FROM categories
        WHERE categories.parent_id = {0}
    |] [q pid]

    checkNotExist pid "категорию" "черновики" $ template [sql|
        SELECT drafts.id, contents.name FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id   
        LEFT JOIN categories ON categories.id = contents.category_id  
        WHERE categories.id = {0}
    |] [q pid]

    checkNotExist pid "категорию" "посты" $ template [sql|
        SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id   
        LEFT JOIN categories ON categories.id = contents.category_id  
        WHERE categories.id = {0}
    |] [q pid]

    delete Category [sql|DELETE FROM categories WHERE id = {0}|] [q pid] 

--Каскадное удаление. Удаляется тег и все привязки тега к контенту
tag :: Int -> T ()
tag pid = do
    execute_ [sql|DELETE FROM tags_to_contents WHERE tag_id = {0}|] [q pid] 
    delete Tag [sql|DELETE FROM tags WHERE id = {0}|] [q pid] 

-- используется для категорий
checkNotExist :: Int -> String -> String -> Query -> T() 
checkNotExist pid name1 name2 templ = do
    results <- query_ $ template templ [q pid] :: T [(Int, String)]
    case results of
        [] -> return ()
        _ -> throwT $ DBError  (template "Невозможно удалить {0}, так как к нему привязаны следующие {1}:\n{2}" [name1, name2, showResults]) where
            showResults = concatMap helper results
            helper :: (Int, String) -> String 
            helper (pid2, name2) = template "id = {0}, name = {1}\n" [show pid2, name2]
