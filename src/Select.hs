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

----------------------------------User-----------------------------------------------------------
type User =  Row.User 
usersQuery :: Int -> Identity Query
usersQuery page = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination page

        selectQuery :: SQL.Query
        selectQuery = [sql|SELECT * FROM users|]

        conditions :: [SQL.Query]
        conditions = []

--универсальный тип, подходящий для любого select
--type Select = undefined

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User

authorsQuery :: Int -> Identity Query
authorsQuery page = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination page

        selectQuery :: SQL.Query
        selectQuery = [sql|SELECT * FROM authors
                LEFT JOIN users
                ON authors.user_id = users.id|] 

        conditions :: [SQL.Query]
        conditions = []

----------------------------Category-----------------------------------------------------------
--категории возвращаются все без пагинации, считается, что их немного
type Category = Row.Category 
categoriesQuery :: Identity Query
categoriesQuery = return [sql|SELECT * FROM categories|] 

-------------------------Post-------------------------------------------------------------
type Post = Row.Post :. Row.Content :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag



-- postsQuery :: Identity SQL.Query
-- postsQuery = return [sql|
--         SELECT * FROM posts
--             LEFT JOIN contents ON contents.id = posts.content_id
--             LEFT JOIN authors ON authors.id = contents.author_id
--             LEFT JOIN users ON users.id = authors.user_id
--             LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
--             LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
--         |]

--localhost/posts?tags_in=[1,2,5]
--type PostParams = Int :. Params Int  :. Params Int :. Params Date :. Maybe String :. Maybe String
--попробовать сделать корректный перевод строки в запросе и табуляцию
--более универсальный тип для параметра, чтобы представить это в виде списка
postsNewQuery :: [(BSName, Param)] -> Identity SQL.Query
postsNewQuery params = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination page
        
        selectQuery :: SQL.Query
        selectQuery = [sql|
                SELECT * FROM posts
                LEFT JOIN contents ON contents.id = posts.content_id
                LEFT JOIN authors ON authors.id = contents.author_id
                LEFT JOIN users ON users.id = authors.user_id
                LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                LEFT JOIN tags ON tags.id = tags_to_contents.tag_id|]

        --исключить из результирующего запроса лишние TRUE в функции whereAll ?
        conditions :: [SQL.Query]
        conditions = map ($ params) [
                -- postIdsSubquery $ jlookup "tag" params,
                -- categoriesCond $ jlookup "category" params,
                -- createdAtCond createdAt,
                -- nameCond mname,
                -- authorNameCond mauthorName,
                -- textCond mtext
                cond [sql|contents.name|] "name"
                cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] "author_name",
                cond [sql|contents.text|] "text"
                ]

        -- postIdsSubquery :: Param -> SQL.Query
        -- postIdsSubquery (ParamsAll tagIds) = [sql|posts.id|] `inSubquery` 
        --         ([sql|SELECT posts.id FROM posts|] `whereAll` map (\tagId -> postTagsSubquery [tagId]) tagIds)
        -- postIdsSubquery (ParamsIn tagIds) = [sql|posts.id|] `inSubquery`
        --         ([sql|SELECT posts.id FROM posts|] `whereAll` [postTagsSubquery tagIds])
        -- postIdsSubquery ParamsAny = [sql|TRUE|]

        -- postTagsSubquery :: [Int] -> SQL.Query
        -- postTagsSubquery tagIds = exists $
        --         [sql| SELECT 1 FROM contents
        --                 LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        --                 WHERE contents.id = posts.content_id
        --                 AND tags_to_contents.tag_id
        --         |] `inList` tagIds
        
        -- categoriesCond :: Param -> SQL.Query
        -- categoriesCond ParamsAny = [sql|TRUE|]
        -- categoriesCond (ParamsIn categoryIds) = [sql|contents.category_id|] `inList` categoryIds
        -- categoriesCond (ParamsAll categoryIds) =  error "this pattern should not occur!" 

        -- --добавить еще паттерн ParamsEQ для удобства
        -- createdAtCond :: Param -> SQL.Query
        -- createdAtCond ParamsAny = [sql|TRUE|]
        -- createdAtCond (ParamsIn [date]) = template [sql|contents.creation_date = '{0}'|] [q date]
        -- createdAtCond (ParamsGT date) = template [sql|contents.creation_date > '{0}'|] [q date]
        -- createdAtCond (ParamsLT date) = template [sql|contents.creation_date < '{0}'|] [q date]
        -- -- createdAtCondTempl :: Date -> SQL.Query -> SQL.Query
        -- -- createdAtCondTempl date sign = template [sql|contents.creation_date {1} '{0}'|] [q date, sign]

        -- nameCond :: Param -> SQL.Query
        -- nameCond Nothing = [sql|TRUE|]
        -- --authorNameCond (Just text) = template [sql|users.first_name + ' ' + users.last_name ILIKE '%{0}%'|] [q text]
        -- nameCond (Just name) = template [sql|contents.name ILIKE '%{0}%'|] [q name]

        -- authorNameCond :: Param -> SQL.Query
        -- authorNameCond Nothing = [sql|TRUE|]
        -- authorNameCond (Just authorName) = template [sql|CONCAT_WS(' ', users.last_name, users.first_name) ILIKE '%{0}%'|] [q authorName]

        


        -- textCond :: Param -> SQL.Query
        -- textCond Nothing = [sql|TRUE|]
        -- textCond (Just text) = template [sql|contents.text ILIKE '%{0}%'|] [q text]



-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 

tagsQuery :: Int -> Params -> Identity Query
tagsQuery page = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination page

        selectQuery :: SQL.Query
        selectQuery = [sql|SELECT * FROM tags|]

        conditions :: [SQL.Query]
        conditions = []

-------------------------Pagination------------------------------------------------------
pagination :: Int -> SQL.Query
pagination page = template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
        quantity = 20

--------------------------Templates-------------------------------------------------------

val :: Val -> Query 
val (Int a) = q a
val (Str a) = template [sql|'{0}'|] [q a]
val (Date a) = template [sql|'{0}'|] [q a]


--data Param = ParamEq Val | ParamIn [Val] | ParamAll [Val] | 
--ParamLt Val | ParamGt Val | ParamBt (Val, Val) | ParamLike Val | ParamNo  deriving Show
cond :: Query -> BSName -> [(BSName, Param)] -> Query 
cond field paramName params = helper param where
        param = jlookup paramName params
        helper (ParamEq v) = template [sql|{0} = {1}|] [field, val v]
        helper (ParamIn list) = field `inList` map val list
        helper (ParamAll list) = error "Нет шаблона запроса для param__all"
        helper (ParamLt v) = template [sql|{0} < {1}|] [field, val v]
        helper (ParamGt v) = template [sql|{0} < {1}|] [field, val v]
        helper (ParamBt (v1,v2)) = template [sql|{0} BETWEEN {1} AND {2}|] [field, val v1, val v2]
        helper (ParamLike (Str s)) = template [sql|{0} ILIKE '%{1}%'|] [field, q s]
        helper ParamNo = [sql|TRUE|]  --это только для Select, для других запросов может быть по другому!!!!
        helper param = error $ template "Нет шаблона для {0}" [show param]




