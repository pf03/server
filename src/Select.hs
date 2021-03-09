{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RecordWildCards #-}
module Select where

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
import qualified Data.ByteString as BC
import Control.Monad.Identity
import Data.Map as M ((!))
import Data.Maybe
----------------------------------User-----------------------------------------------------------
type User =  Row.User 

user :: Int -> T (Maybe User)
user pid = listToMaybe <$> query_ query where
        query =  selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [q pid]

users :: ParamsMap Param -> T [User]
users params = query_ $ usersQuery params

--отделение чистого кода от грязного
selectUsersQuery :: Query 
selectUsersQuery = [sql|SELECT * FROM users|]

usersQuery :: ParamsMap Param -> Query
usersQuery params = selectUsersQuery <+> pagination (params ! "page")

-- checkUser :: Param -> T Bool
-- checkUser param = isJust <$> Select.user param

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User

author :: Int -> T (Maybe Author)
author pid = listToMaybe <$> query_ query where
        query =  selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [q pid]

authors :: ParamsMap Param -> T [Author]
authors params = query_ $ authorsQuery params

selectAuthorsQuery :: SQL.Query
selectAuthorsQuery = [sql|SELECT * FROM authors
                LEFT JOIN users
                ON authors.user_id = users.id|] 

authorsQuery :: ParamsMap Param -> Query
authorsQuery params = selectAuthorsQuery <+> pagination (params ! "page")

----------------------------Category-----------------------------------------------------------
--категории возвращаются все без пагинации, считается, что их немного
--можно сделать запрос без пагинации для внутреннего использования и с пагинацией для внешнего
type Category = Row.Category 

category::  Int -> T (Maybe Category)
category pid = listToMaybe <$> query_ query where
        query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

categories :: ParamsMap Param -> T [Category]
categories params = query_ $ categoriesQuery params

--все категории без пагинации нужны для вычисления родительских категорий
allCategories :: T [Category]
allCategories = query_ selectCategoriesQuery

selectCategoriesQuery ::  Query
selectCategoriesQuery = [sql|SELECT * FROM categories|] 

categoriesQuery :: ParamsMap Param -> Query
categoriesQuery params = selectCategoriesQuery <+> pagination (params ! "page")

-------------------------Post-------------------------------------------------------------
type Post = Row.Post :. Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag

--не все так просто!!! Из-за того, что у одного поста может быть много тегов, здесь может быть много строк!!!
--В общем случае может быть много строк, а на выходе 0 или 1 объект json!!
-- post::  Int -> T (Maybe Post)
-- post pid = listToMaybe <$> query_ query where
--         query = selectPostsQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

post::  Int -> T [Post]
post pid = query_ query where
        query = selectPostsQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

posts :: ParamsMap Param -> T [Post]
posts params = query_ $ postsQuery params

--зачем здесь таблица categories? проверить!!!
selectPostsQuery ::  Query
selectPostsQuery = [sql|
        SELECT * FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id|]

postsQuery :: ParamsMap Param -> SQL.Query
postsQuery params = res where

        p :: BSName -> Param
        p name = params ! name

        res:: SQL.Query
        res = selectPostsQuery `whereAll` conditions  <+> orderBy (p "order_by") <+> pagination (p "page")

        --исключить из результирующего запроса лишние TRUE в функции whereAll ?
        conditions :: [SQL.Query]
        conditions =  [
                        postIdsSubquery [sql|tags_to_contents.tag_id|] (p "tag_id"),
                        containsCond (p "contains"),
                        cond [sql|contents.category_id|] $ p "category_id",
                        cond [sql|contents.creation_date|] $ p "created_at",
                        cond [sql|contents.name|] $ p "name",
                        cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ p "author_name",
                        cond [sql|contents.text|] $ p "text"
                ]

        postIdsSubquery :: Query -> Param -> SQL.Query
        
        postIdsSubquery field (ParamAll vals) = [sql|posts.id|] `inSubquery` 
                ([sql|SELECT posts.id FROM posts|] `whereAll` map (\tagId -> existTagSubquery field $ ParamEq tagId) vals)
        postIdsSubquery _ ParamNo = [sql|TRUE|]
        --postIdsSubquery (ParamEq val) = postIdsSubquery (ParamIn [val])
        postIdsSubquery field param = [sql|posts.id|] `inSubquery`
                ([sql|SELECT posts.id FROM posts|] `whereAll` [existTagSubquery field param] )


        -- postTagsSubquery :: [Val] -> SQL.Query
        -- postTagsSubquery tagIds = exists $
        --         [sql| SELECT 1 FROM contents
        --                 LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        --                 WHERE contents.id = posts.content_id
        --                 AND tags_to_contents.tag_id
        --         |] `inList` map val tagIds

        containsCond :: Param -> SQL.Query
        containsCond param = Query.brackets . Query.any $ list where
                list = [
                        cond [sql|contents.name|] param,
                        cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
                        cond [sql|contents.name|] param,
                        cond [sql|categories.category_name|] param,
                        postIdsSubquery [sql|tags.name|] param
                        ]

        --containsCond param = error $ template "Нет шаблона для {0}" [show param]

        --поиск по имени и id тега
        --вытягиваем id постов, в которых хотя бы один из тегов удовлетворяет условию. После чего возвращаем все теги постов с данными id
        existTagSubquery :: Query -> Param -> Query
        existTagSubquery field param  = exists $ 
                [sql| SELECT 1 FROM contents
                        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
                        WHERE contents.id = posts.content_id
                        AND |] <+> cond field param
        
        orderBy :: Param -> SQL.Query
        orderBy (ParamEq (Str paramName)) = template [sql|ORDER BY {0}|] [field] where
                field = case paramName of 
                        "created_at" -> [sql|contents.creation_date|]
                        "author_name" -> [sql|CONCAT_WS(' ', users.last_name, users.first_name)|]
                        "category_id"-> [sql|contents.category_id|]
                        "photos" -> Query.brackets [sql|SELECT COUNT(*) FROM photos WHERE
                                photos.content_id = contents.id|]

-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 
tag::  Int -> T (Maybe Tag)
tag pid = listToMaybe <$> query_ query where
        query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [q pid]

tags :: ParamsMap Param -> T [Tag]
tags params = query_ $ tagsQuery params

selectTagsQuery ::  Query
selectTagsQuery = [sql|SELECT * FROM tags|] 

tagsQuery :: ParamsMap Param -> Query
tagsQuery params = selectTagsQuery <+> pagination (params ! "page")


-------------------------Pagination------------------------------------------------------
pagination :: Param -> SQL.Query
pagination (ParamEq (Int page)) = template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
        quantity = 20

------------------------Sort-------------------------------------------------------------
--возможно сортировка должна быть разная для разных запросов




--------------------------Templates-------------------------------------------------------
-- c этой функцией нужно осторожно, так как она работает только для ParamEq
--возможно здесь сделать обработку ошибок для некорректных паттернов??
p :: Param -> Query
p (ParamEq v) = val v
p _ = error "Функция p работает только для ParamEq"

val :: Val -> Query 
val (Int a) = q a
val (Str a) = template [sql|'{0}'|] [q a]
val (Date a) = template [sql|'{0}'|] [q a]

--helper можно убрать
cond :: Query -> Param -> Query 
cond field param = helper param where
        --param = jlookup paramName params
        helper (ParamEq v) = template [sql|{0} = {1}|] [field, val v]
        helper (ParamIn list) = field `inList` map val list
        helper (ParamAll list) = error "Нет шаблона запроса для param__all"
        helper (ParamLt v) = template [sql|{0} < {1}|] [field, val v]
        helper (ParamGt v) = template [sql|{0} > {1}|] [field, val v]
        helper (ParamBt (v1,v2)) = template [sql|{0} BETWEEN {1} AND {2}|] [field, val v1, val v2]
        helper (ParamLike (Str s)) = template [sql|{0} ILIKE '%{1}%'|] [field, q s]
        helper ParamNo = [sql|TRUE|]  --это только для Select, для других запросов может быть по другому!!!!
        helper param = error $ template "Нет шаблона для {0}" [show param]




