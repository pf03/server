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

usersQuery1 :: Query 
usersQuery1 = [sql|SELECT * FROM users|]

--будте просто users
usersQuery :: ParamsMap Param ->  Identity Query
usersQuery params = return res where
        res:: SQL.Query
        res = usersQuery1 `whereAll` conditions <+> pagination (params ! "page")

        conditions :: [SQL.Query]
        conditions = []

--это будет отдельная api
user :: Param -> T (Maybe Row.User)
user param = listToMaybe <$> query_ query where
        query =  usersQuery1 <+> template [sql|SELECT * FROM users WHERE users.id = {0}|] [p param]

-- checkUser :: Param -> T Bool
-- checkUser param = isJust <$> Select.user param

--универсальный тип, подходящий для любого select
--type Select = undefined

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User

authorsQuery :: ParamsMap Param ->  Identity Query
authorsQuery params = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination (params ! "page")

        selectQuery :: SQL.Query
        selectQuery = [sql|SELECT * FROM authors
                LEFT JOIN users
                ON authors.user_id = users.id|] 

        conditions :: [SQL.Query]
        conditions = []

----------------------------Category-----------------------------------------------------------
--категории возвращаются все без пагинации, считается, что их немного
--можно сделать запрос без пагинации для внутреннего использования и с пагинацией для внешнего
type Category = Row.Category 
categoriesQuery ::  Identity Query
categoriesQuery = return [sql|SELECT * FROM categories|] 

--возможно здесь передать просто номер категории вместо сложного объекта? тогда нарушится универсальность
--и сложно будет вызывать из модуля db, будут костыли
category::  Param -> T (Maybe Row.Category)
category param = listToMaybe <$> query_ query where
        query =  template [sql|SELECT * FROM categories WHERE categories.id = {0}|] [p param]





-------------------------Post-------------------------------------------------------------
type Post = Row.Post :. Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag



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
--возможно сделать здесь обработку некорректных шаблонов
postsNewQuery :: ParamsMap Param -> Identity SQL.Query
postsNewQuery params = return res where

        p :: BSName -> Param
        p name = params ! name

        res:: SQL.Query
        res = selectQuery `whereAll` conditions  <+> orderBy (p "order_by") <+> pagination (p "page")
        

        --зачем здесь таблица categories? проверить!!!
        selectQuery :: SQL.Query
        selectQuery = [sql|
                SELECT * FROM posts
                LEFT JOIN contents ON contents.id = posts.content_id
                LEFT JOIN categories ON categories.id = contents.category_id
                LEFT JOIN authors ON authors.id = contents.author_id
                LEFT JOIN users ON users.id = authors.user_id
                LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                LEFT JOIN tags ON tags.id = tags_to_contents.tag_id|]

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

tagsQuery :: ParamsMap Param -> Identity Query
tagsQuery params = return res where
        res:: SQL.Query
        res = selectQuery `whereAll` conditions <+> pagination (params ! "page")

        selectQuery :: SQL.Query
        selectQuery = [sql|SELECT * FROM tags|]

        conditions :: [SQL.Query]
        conditions = []

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




