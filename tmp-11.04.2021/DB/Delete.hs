{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Delete where

import Data.Text (pack, Text(..))
import qualified DB.Types.Row as Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common.Common
import qualified DB.Class as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Identity
import Data.Map as M ((!), fromList)
import Control.Monad.Trans.Except
import qualified Log.Class as Log
import Data.Maybe
import Data.Monoid
import Data.Int
import Select
import Update


-- | Удаление 4 типов
-- 1. Удаленная сущность заменяется на значение по умолчанию. Используется для users и authors
-- 2. Каскадное удаление вместе с привязанными сущностями. Используется для posts и drafts
-- 3. Удаление строго по условию, если к данной сущности ничего не привязано. Сначала нужно отредактировать или удалить 
-- связанные сущности, а потом продолжить удаление. Используется для categories.
-- 4. Простое удаление, если от сущности ничего не зависит. Используется для comments

--юзер удаляется, автор привязывается к дефолтному юзеру, авторизация админ на уровне роутера
user :: MT m => Int -> m ()
user pid = do 
    when (pid == 1) $ Error.throw $ DBError "Невозможно удалить пользователя по умолчанию с id = 1"
    when (pid == 2) $ Error.throw $ DBError "Невозможно удалить админа с id = 2"
    DB.update Author [sql|UPDATE authors SET user_id = 1 WHERE user_id = {0}|] [q pid]
    DB.update Comment [sql|UPDATE comments SET user_id = 1 WHERE user_id = {0}|] [q pid]
    DB.delete User [sql|DELETE FROM users WHERE id = {0}|] [q pid]

--юзер удаляется, автор привязывается к дефолтному юзеру, авторизация админ на уровне роутера
author :: MT m => Int -> m ()
author pid = do 
    when (pid == 1) $ Error.throw $ DBError "Невозможно удалить автора по умолчанию с id = 1"
    DB.update Content [sql|UPDATE contents SET author_id = 1 WHERE author_id = {0}|] [q pid] 
    DB.delete Author [sql|DELETE FROM authors WHERE id = {0}|] [q pid]

--для поста каскадное удаление
post :: MT m => Int -> m () 
post pid = do 
    checkAuthExistPost pid
    ParamEq (Int cid) <- Cache.getParam "content_id" 
    DB.delete Post [sql|DELETE FROM posts WHERE id = {0}|] [q pid]   
    DB.delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q cid]   
    DB.delete Draft [sql|DELETE FROM drafts WHERE post_id = {0}|] [q pid] 
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q cid]   
    DB.delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q cid]   
    DB.delete Comment [sql|DELETE FROM comments WHERE post_id = {0}|] [q pid]

--для черновика каскадное удаление
draft :: MT m => Int -> m ()
draft pid = do
    checkAuthExistDraft pid
    ParamEq (Int cid) <- Cache.getParam "content_id" 
    DB.delete Draft [sql|DELETE FROM drafts WHERE id = {0}|] [q pid]   
    DB.delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q cid]   
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q cid]   
    DB.delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q cid]

comment :: MT m => Int -> m ()
comment pid = do 
    checkAuthExistComment pid
    DB.delete Comment [sql|DELETE FROM comments WHERE id = {0}|] [q pid] 

--удаление строго по условию, если не привязаны другие категории и контент
category :: MT m => Int -> m ()
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

    DB.delete Category [sql|DELETE FROM categories WHERE id = {0}|] [q pid] 

--Каскадное удаление. Удаляется тег и все привязки тега к контенту
tag :: MT m => Int -> m ()
tag pid = do
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE tag_id = {0}|] [q pid] 
    DB.delete Tag [sql|DELETE FROM tags WHERE id = {0}|] [q pid] 

-- используется для категорий
checkNotExist :: MDB m => Int -> String -> String -> Query -> m () 
checkNotExist pid name1 name2 templ = do
    results <- DB.query_ $ template templ [q pid] 
    case results :: [(Int, String)] of
        [] -> return ()
        _ -> Error.throw $ DBError  (template "Невозможно удалить {0}, так как к нему привязаны следующие {1}:\n{2}" [name1, name2, showResults]) where
            showResults = concatMap helper results
            helper :: (Int, String) -> String 
            helper (pid2, name2) = template "id = {0}, name = {1}\n" [show pid2, name2]
