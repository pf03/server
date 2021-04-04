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
import qualified State as S
import Transformer
import Error

--Можно добавить еще пару классов MCache (чистая монада), MDB (грязная)
----------------------------------User-----------------------------------------------------------
type User =  Row.User 

user :: Int -> T (Maybe User)
user pid = listToMaybe <$> query_ query where
        query =  selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [q pid]

users :: T [User]
users = query_ =<< usersQuery =<< S.getParams

--отделение чистого кода от грязного
selectUsersQuery :: Query 
selectUsersQuery = [sql|SELECT * FROM users|]

usersQuery ::  MError m => ParamsMap Param -> m Query
usersQuery params = return selectUsersQuery <<+>> pagination (params ! "page")

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User

author :: Int -> T (Maybe Author)
author pid = listToMaybe <$> query_ query where
        query =  selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [q pid]

authors :: T [Author]
authors = query_ =<< authorsQuery =<< S.getParams

selectAuthorsQuery :: SQL.Query
selectAuthorsQuery = [sql|SELECT * FROM authors
                LEFT JOIN users
                ON authors.user_id = users.id|] 

authorsQuery :: MError m => ParamsMap Param -> m Query
authorsQuery params = return selectAuthorsQuery <<+>> pagination (params ! "page")

----------------------------Category-----------------------------------------------------------
--запрос без пагинации для внутреннего использования и с пагинацией для внешнего
type Category = Row.Category 

category::  Int -> T (Maybe Category)
category pid = listToMaybe <$> query_ query where
        query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

categories :: T [Category]
categories = query_ =<< categoriesQuery =<< S.getParams

--все категории без пагинации нужны для вычисления родительских категорий
allCategories :: T [Category]
allCategories = query_ selectCategoriesQuery

selectCategoriesQuery ::  Query
selectCategoriesQuery = [sql|SELECT * FROM categories|] 

categoriesQuery :: MError m => ParamsMap Param -> m Query
categoriesQuery params = return selectCategoriesQuery <<+>> pagination (params ! "page")

-------------------------Draft-------------------------------------------------------------
type Content = Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag :. Maybe Row.Photo

type Draft = Row.Draft :. Select.Content

draft::  Int -> T [Draft]
draft pid = do
    paramUserId <- authUserIdParam
    conditions <-  sequenceA [
        cond [sql|drafts.id|] $ ParamEq (Int pid),
        cond [sql|users.id|] paramUserId
        ]
    let query =  selectDraftsQuery `whereAll` conditions;
    query_ query

drafts :: T [Draft]
drafts = do
    params <- S.getParams
    paramUserId <- authUserIdParam
    let conditions = [
            cond [sql|users.id|] paramUserId
            ]
    query <- selectDraftsQuery `whereAllM` conditions <<+>> pagination (params ! "page");
    query_ query

selectDraftsQuery ::  Query
selectDraftsQuery = [sql|
    SELECT * FROM drafts
    LEFT JOIN contents ON contents.id = drafts.content_id
    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
    LEFT JOIN photos ON photos.content_id = contents.id|]

-- draftsQuery :: ParamsMap Param -> Query
-- draftsQuery params = selectDraftsQuery <+> pagination (params ! "page")

-------------------------Post-------------------------------------------------------------

type Post = Row.Post :. Select.Content
--type Post = Row.Post :. Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag

-- post::  Int -> T (Maybe Post)
-- post pid = listToMaybe <$> query_ query where
--         query = selectPostsQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

post::  Int -> T [Post]
post pid = query_ query where
        query = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [q pid]

posts :: T [Post]
posts = query_ =<< postsQuery =<< S.getParams

--зачем здесь таблица categories? проверить!!!
selectPostsQuery ::  Query
selectPostsQuery = [sql|
        SELECT * FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
        LEFT JOIN photos ON photos.content_id = contents.id|]

postsQuery :: MError m => ParamsMap Param -> m SQL.Query
postsQuery params = res
    where

    p :: BSName -> Param
    p name = params ! name

    --перенести в ду нотацию
    res :: MError m => m SQL.Query
    res = selectPostsQuery `whereAllM` conditions  <<+>> orderBy (p "order_by") <<+>> pagination (p "page")

    --исключить из результирующего запроса лишние TRUE в функции whereAll ?
    conditions :: MError m => [m SQL.Query]
    conditions =  [
        postIdsSubquery [sql|tags_to_contents.tag_id|] (p "tag_id"),
        containsCond (p "contains"),
        cond [sql|contents.category_id|] $ p "category_id",
        cond [sql|contents.creation_date|] $ p "created_at",
        cond [sql|contents.name|] $ p "name",
        cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ p "author_name",
        cond [sql|contents.text|] $ p "text"
        ]

    postIdsSubquery :: MError m => Query -> Param -> m SQL.Query
    
    postIdsSubquery field (ParamAll vals) = [sql|posts.id|] `inSubqueryM` 
            ([sql|SELECT posts.id FROM posts|] `whereAllM` map (existTagSubquery field . ParamEq ) vals)
    postIdsSubquery _ ParamNo = return [sql|TRUE|]
    --postIdsSubquery (ParamEq val) = postIdsSubquery (ParamIn [val])
    postIdsSubquery field param = [sql|posts.id|] `inSubqueryM`
            ([sql|SELECT posts.id FROM posts|] `whereAllM` [existTagSubquery field param] )

    containsCond :: MError m => Param -> m SQL.Query
    containsCond param = Query.brackets . Query.any <$> list where
        list = sequenceA [
            cond [sql|contents.name|] param,
            cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
            cond [sql|contents.name|] param,
            cond [sql|categories.category_name|] param,
            postIdsSubquery [sql|tags.name|] param
            ]

    --containsCond param = error $ template "Нет шаблона для {0}" [show param]

    --поиск по имени и id тега
    --вытягиваем id постов, в которых хотя бы один из тегов удовлетворяет условию. После чего возвращаем все теги постов с данными id
    existTagSubquery :: MError m => Query -> Param -> m Query
    existTagSubquery field param  = do 
        c <- cond field param
        return $ exists $ 
            [sql| SELECT 1 FROM contents
                    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
                    WHERE contents.id = posts.content_id
                    AND |] <+> c
    
    orderBy :: MError m => Param -> m SQL.Query
    orderBy (ParamEq (Str paramName)) = return . template [sql|ORDER BY {0}|] <$$> [field] where
        field = case paramName of 
            "created_at" -> return [sql|contents.creation_date|]
            "author_name" -> return [sql|CONCAT_WS(' ', users.last_name, users.first_name)|]
            "category_id"-> return [sql|contents.category_id|]
            "photos" -> return $ Query.brackets [sql|SELECT COUNT(*) FROM photos WHERE
                    photos.content_id = contents.id|]
            _ -> throwM $ DevError $ template "Неверный параметр {0} в функции orderBy" [paramName]
    orderBy ParamNo = return [sql||]
    orderBy p = throwM $ DevError $ template "Неверный шаблон параметра {0} в функции orderBy" [show p]

-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 
tag::  Int -> T (Maybe Tag)
tag pid = listToMaybe <$> query_ query where
        query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [q pid]

tags :: T [Tag]
tags = query_ =<< tagsQuery =<< S.getParams

selectTagsQuery ::  Query
selectTagsQuery = [sql|SELECT * FROM tags|] 

--здесь можно добавить MonadCache
tagsQuery :: MError m => ParamsMap Param -> m Query
tagsQuery params = return selectTagsQuery <<+>> pagination (params ! "page")

-------------------------Comment----------------------------------------------------------

type Comment =  Row.Comment :. Row.Post :. Row.User
-- comments::  Int -> T (Maybe Tag)
-- comments postId = listToMaybe <$> query_ query where
--         query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [q pid]

comments :: Int -> T [Comment]
comments postId = query_ =<< commentsQuery postId =<< S.getParams

selectCommentsQuery ::  Query
selectCommentsQuery = [sql|
    SELECT * FROM comments 
        LEFT JOIN posts ON posts.id = comments.post_id
        LEFT JOIN users ON users.id = comments.user_id
    |] 

commentsQuery :: MError m => Int -> ParamsMap Param -> m Query
commentsQuery postId params = selectCommentsQuery `whereAllM` (return <$> conditions) <<+>> pagination (params ! "page") where
    conditions :: [SQL.Query]
    conditions =  [
        template [sql|posts.id = {0}|] [q postId]
        ]



-------------------------Pagination--------------------------------------------------------
pagination :: MError m => Param -> m SQL.Query
pagination (ParamEq (Int page)) = return $ template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
    quantity = 20
pagination p = throwM $ DevError $ template "Неверный шаблон параметра {0} в функции pagination" [show p]



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

cond :: MError m => Query -> Param -> m Query
cond field param = case param of
    ParamEq v -> return $ template [sql|{0} = {1}|] [field, val v]
    ParamIn list -> return $ field `inList` map val list
    --ParamAll list -> error "Нет шаблона запроса для param__all"
    ParamLt v -> return $ template [sql|{0} < {1}|] [field, val v]
    ParamGt v -> return $ template [sql|{0} > {1}|] [field, val v]
    ParamBt (v1,v2) -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, val v1, val v2]
    --ParamLike (Str s) -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, q s]
    ParamLike v -> return $ template [sql|{0} = {1}|] [field, val v] --ilike только для строки, для остальных равенство
    ParamNo -> return $ [sql|TRUE|]  --это только для Select, для других запросов может быть по другому!!!!
    ParamNull -> return $ template [sql|{0} = null|] [field] --не проверено
    _ -> throwM $ DevError $ template "Неверный шаблон параметра {0} в функции cond" [show param]

--админ может ЧИТАТЬ все публикации
authUserIdParam :: T Param
authUserIdParam = do
    auth <- S.getAuth
    case auth of
        AuthAdmin _ -> return ParamNo
        AuthUser userId -> return $ ParamEq (Int userId)
        _ -> throwT authErrorDefault




