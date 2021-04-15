{-# LANGUAGE TypeOperators #-}
module Logic.DB.Select 
    ( User, user, users
    , Author, author, authors 
    , Category, category, categories, allCategories 
    , Tag, tag, tags
    , Draft, draft, drafts
    , Post, post, posts
    , Comment, comments
    , p, val, cond, authUserIdParam
    ) where

-- Our Modules
import           Common.Misc
import           Interface.Cache                  as Cache
import           Interface.DB                     as DB
import           Interface.Error                  as Error
import qualified Logic.DB.Row                     as Row

-- Other Modules
import           Data.Map                         as M ((!))
import           Data.Maybe
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types as SQL

-----------------------------User----------------------------------------------
type User =  Row.User

user :: MDB m => Int -> m (Maybe User)
user pid = listToMaybe <$> DB.query_ query where
    query =  selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [q pid]

users :: MT m => m [User]
users = DB.query_ =<< usersQuery =<< Cache.getParams

selectUsersQuery :: Query
selectUsersQuery = [sql|SELECT * FROM users|]

usersQuery ::  (MError m, MCache m) => ParamsMap Param -> m Query
usersQuery params = return selectUsersQuery <<+>> pagination

-----------------------------Author--------------------------------------------
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

-----------------------------Category------------------------------------------
type Category = Row.Category

category::  MDB m => Int -> m (Maybe Category)
category pid = listToMaybe <$> DB.query_ query where
    query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [q pid]

categories :: MT m => m [Category]
categories = DB.query_ =<< categoriesQuery

-- * Все категории без пагинации нужны для вычисления родительских категорий
allCategories :: MDB m => m [Category]
allCategories = DB.query_ selectCategoriesQuery

selectCategoriesQuery ::  Query
selectCategoriesQuery = [sql|SELECT * FROM categories|]

categoriesQuery :: (MError m, MCache m) => m Query
categoriesQuery = return selectCategoriesQuery <<+>> pagination

-----------------------------Draft---------------------------------------------
type Content = Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag :. Maybe Row.Photo
type Draft = Row.Draft :. Content

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
    params <- Cache.getParams
    paramUserId <- authUserIdParam
    let conditions = [cond [sql|users.id|] paramUserId]
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

-----------------------------Post----------------------------------------------
type Post = Row.Post :. Content

post::  MDB m => Int -> m [Post]
post pid = DB.query_ query where
    query = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [q pid]

posts :: MT m => m [Post]
posts = DB.query_ =<< postsQuery

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

postsQuery :: (MError m, MCache m) => m SQL.Query
postsQuery = do
    params <- Cache.getParams
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

        -- | Поиск по имени и id тега
        -- Вытягиваем id постов, в которых хотя бы один из тегов удовлетворяет условию.
        -- После чего возвращаем все теги постов с данными id
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
                "created_at"    -> return [sql|contents.creation_date|]
                "author_name"   -> return [sql|CONCAT_WS(' ', users.last_name, users.first_name)|]
                "category_id"   -> return [sql|contents.category_id|]
                "photos"        -> return $ DB.brackets [sql|SELECT COUNT(*) FROM photos WHERE
                    photos.content_id = contents.id|]
                _               -> Error.throw $ DevError $ template "Неверный параметр {0} в функции orderBy" [paramName]
        orderBy ParamNo = return [sql||]
        orderBy p = Error.throw $ DevError $ template "Неверный шаблон параметра {0} в функции orderBy" [show p]

-----------------------------Tag-----------------------------------------------
type Tag = Row.Tag
tag::  MDB m => Int -> m (Maybe Tag)
tag pid = listToMaybe <$> DB.query_ query where
    query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [q pid]

tags :: MT m => m [Tag]
tags = DB.query_ =<< tagsQuery

selectTagsQuery ::  Query
selectTagsQuery = [sql|SELECT * FROM tags|]

tagsQuery :: (MError m, MCache m) => m Query
tagsQuery  = return selectTagsQuery <<+>> pagination

-----------------------------Comment-------------------------------------------

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

-----------------------------Pagination----------------------------------------
pagination :: (MError m, MCache m) => m SQL.Query
pagination  = do
    paramPage <- Cache.getParam "page"
    case paramPage of
        ParamEq (Int page)  -> return $ template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
            quantity = 20
        _                   -> Error.throw $ DevError $ template "Неверный шаблон параметра {0} в функции pagination" [show paramPage]

-----------------------------Templates-----------------------------------------
p :: MError m => Param -> m Query
p (ParamEq v) = return $ val v
p param       = Error.throw $ DevError $ template "Неверный шаблон параметра {0} в функции p" [show param]

val :: Val -> Query
val (Int a)  = q a
val (Str a)  = template [sql|'{0}'|] [q a]
val (Date a) = template [sql|'{0}'|] [q a]

cond :: MError m => Query -> Param -> m Query
cond field param = case param of
    ParamEq v           -> return $ template [sql|{0} = {1}|] [field, val v]
    ParamIn list        -> return $ field `inList` map val list
    ParamLt v           -> return $ template [sql|{0} < {1}|] [field, val v]
    ParamGt v           -> return $ template [sql|{0} > {1}|] [field, val v]
    ParamBt (v1,v2)     -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, val v1, val v2]
    ParamLike (Str s)   -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, q s]
    ParamLike v         -> return $ template [sql|{0} = {1}|] [field, val v]
    ParamNo             -> return [sql|TRUE|]
    ParamNull           -> return $ template [sql|{0} = null|] [field]
    _                   -> Error.throw $ DevError $ template "Неверный шаблон параметра {0} в функции cond" [show param]

-- * Админ может ЧИТАТЬ все публикации
authUserIdParam :: (MError m, MCache m) => m Param
authUserIdParam = do
    auth <- Cache.getAuth
    case auth of
        AuthAdmin _     -> return ParamNo
        AuthUser userId -> return $ ParamEq (Int userId)
        _               -> Error.throw Error.authErrorDefault




