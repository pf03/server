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
import Types hiding (T) --T перенести в трансформер
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
--import Transformer
import Error
--import qualified DB
import DB --((<<+>>))

--Можно добавить еще пару классов MCache (чистая монада), MDB (грязная)
----------------------------------User-----------------------------------------------------------
type User =  Row.User 

user :: MDB m => Int -> m (Maybe User)
user pid = listToMaybe <$> DB.query_ query where
        query =  selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [q pid]

users :: MT m => m [User]
users = DB.query_ =<< usersQuery =<< S.getParams

--отделение чистого кода от грязного
selectUsersQuery :: Query 
selectUsersQuery = [sql|SELECT * FROM users|]

usersQuery ::  (MError m, MCache m) => ParamsMap Param -> m Query
usersQuery params = return selectUsersQuery <<+>> pagination

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User

author :: MDB m => Int -> m (Maybe Author)
author pid = listToMaybe <$> DB.query_ query where
        query =  selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [q pid]

authors :: MT m => m [Author]
authors = DB.query_ =<< authorsQuery

selectAuthorsQuery :: SQL.Query
selectAuthorsQuery = [sql|SELECT * FROM authors
                LEFT JOIN users
                ON authors.user_id = users.id|] 

authorsQuery :: (MError m, MCache m) => m Query
authorsQuery = return selectAuthorsQuery <<+>> pagination

----------------------------Category-----------------------------------------------------------
--запрос без пагинации для внутреннего использования и с пагинацией для внешнего
type Category = Row.Category 

category::  MDB m => Int -> m (Maybe Category)
category pid = listToMaybe <$> DB.query_ query where
        query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

categories :: MT m => m [Category]
categories = DB.query_ =<< categoriesQuery

--все категории без пагинации нужны для вычисления родительских категорий
allCategories :: MDB m => m [Category]
allCategories = DB.query_ selectCategoriesQuery

selectCategoriesQuery ::  Query
selectCategoriesQuery = [sql|SELECT * FROM categories|] 

categoriesQuery :: (MError m, MCache m) => m Query
categoriesQuery = return selectCategoriesQuery <<+>> pagination

-------------------------Draft-------------------------------------------------------------
type Content = Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag :. Maybe Row.Photo

type Draft = Row.Draft :. Select.Content

draft::  MT m => Int -> m [Draft]
draft pid = do
    paramUserId <- authUserIdParam
    conditions <-  sequenceA [
        cond [sql|drafts.id|] $ ParamEq (Int pid),
        cond [sql|users.id|] paramUserId
        ]
    let query =  selectDraftsQuery `whereAll` conditions;
    DB.query_ query

drafts :: MT m => m [Draft]
drafts = do
    params <- S.getParams
    paramUserId <- authUserIdParam
    let conditions = [
            cond [sql|users.id|] paramUserId
            ]
    query <- selectDraftsQuery `whereAllM` conditions <<+>> pagination;
    DB.query_ query

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

-- post::  Int -> m (Maybe Post)
-- post pid = listToMaybe <$> DB.query_ query where
--         query = selectPostsQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

post::  MDB m => Int -> m [Post]
post pid = DB.query_ query where
        query = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [q pid]

posts :: MT m => m [Post]
posts = DB.query_ =<< postsQuery

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

--S.getParams можно использовать на самом низком уровне!!
postsQuery :: (MError m, MCache m) => m SQL.Query
postsQuery = do
    params <- S.getParams
    let p name = params ! name
    let conditions =  [
            postIdsSubquery [sql|tags_to_contents.tag_id|] (p "tag_id"),
            containsCond (p "contains"),
            cond [sql|contents.category_id|] $ p "category_id",
            cond [sql|contents.creation_date|] $ p "created_at",
            cond [sql|contents.name|] $ p "name",
            cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ p "author_name",
            cond [sql|contents.text|] $ p "text"
            ]
    selectPostsQuery `whereAllM` conditions  <<+>> orderBy (p "order_by") <<+>> pagination

        where

        postIdsSubquery :: MError m => Query -> Param -> m Query
        
        postIdsSubquery field (ParamAll vals) = [sql|posts.id|] `inSubqueryM` 
                ([sql|SELECT posts.id FROM posts|] `whereAllM` map (existTagSubquery field . ParamEq ) vals)
        postIdsSubquery _ ParamNo = return [sql|TRUE|]
        --postIdsSubquery (ParamEq val) = postIdsSubquery (ParamIn [val])
        postIdsSubquery field param = [sql|posts.id|] `inSubqueryM`
                ([sql|SELECT posts.id FROM posts|] `whereAllM` [existTagSubquery field param] )

        containsCond :: MError m => Param -> m Query
        containsCond param = DB.brackets . DB.any <$> list where
            list = sequenceA [
                cond [sql|contents.name|] param,
                cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
                cond [sql|contents.name|] param,
                cond [sql|categories.category_name|] param,
                postIdsSubquery [sql|tags.name|] param
                ]

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
                "photos" -> return $ DB.brackets [sql|SELECT COUNT(*) FROM photos WHERE
                        photos.content_id = contents.id|]
                _ -> throwM $ DevError $ template "Неверный параметр {0} в функции orderBy" [paramName]
        orderBy ParamNo = return [sql||]
        orderBy p = throwM $ DevError $ template "Неверный шаблон параметра {0} в функции orderBy" [show p]

-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 
tag::  MDB m => Int -> m (Maybe Tag)
tag pid = listToMaybe <$> DB.query_ query where
        query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [q pid]

tags :: MT m => m [Tag]
tags = DB.query_ =<< tagsQuery

selectTagsQuery ::  Query
selectTagsQuery = [sql|SELECT * FROM tags|] 

--здесь можно добавить MonadCache
tagsQuery :: (MError m, MCache m) => m Query
tagsQuery  = return selectTagsQuery <<+>> pagination

-------------------------Comment----------------------------------------------------------

type Comment =  Row.Comment :. Row.Post :. Row.User

comments :: MT m => Int -> m [Comment]
comments postId = DB.query_ =<< commentsQuery postId

selectCommentsQuery ::  Query
selectCommentsQuery = [sql|
    SELECT * FROM comments 
        LEFT JOIN posts ON posts.id = comments.post_id
        LEFT JOIN users ON users.id = comments.user_id
    |] 

commentsQuery :: (MError m, MCache m) => Int -> m Query
commentsQuery postId = selectCommentsQuery `whereAllM` (return <$> conditions) <<+>> pagination where
    conditions :: [SQL.Query]
    conditions =  [
        template [sql|posts.id = {0}|] [q postId]
        ]



-------------------------Pagination--------------------------------------------------------
pagination :: (MError m, MCache m) => m SQL.Query
pagination  = do
    paramPage <- S.getParam "page"
    case paramPage of 
        ParamEq (Int page) -> return $ template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
            quantity = 20
        _ -> throwM $ DevError $ template "Неверный шаблон параметра {0} в функции pagination" [show paramPage]
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
authUserIdParam :: (MError m, MCache m) => m Param
authUserIdParam = do
    auth <- S.getAuth
    case auth of
        AuthAdmin _ -> return ParamNo
        AuthUser userId -> return $ ParamEq (Int userId)
        _ -> throwM authErrorDefault




