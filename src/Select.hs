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

----------------------------------User-----------------------------------------------------------
type User = Row.User 
usersQuery = [sql|SELECT * FROM users|]

--универсальный тип, подходящий для любого select
--type Select = undefined

-------------------------------Author---------------------------------------------------------
type Author = Row.Author :. Row.User
authorsQuery = [sql|SELECT * FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id|]

----------------------------Category-----------------------------------------------------------
type Category = Row.Category 
categoriesQuery = [sql|SELECT * FROM categories|]

-------------------------Post-------------------------------------------------------------
type Post = Row.Post :. Row.Content :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag

postsQuery :: SQL.Query
postsQuery = [sql|
        SELECT * FROM posts
            LEFT JOIN contents ON contents.id = posts.content_id
            LEFT JOIN authors ON authors.id = contents.author_id
            LEFT JOIN users ON users.id = authors.user_id
            LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
            LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
        |]

--localhost/posts?tags_in=[1,2,5]

--попробовать сделать корректный перевод строки в запросе и табуляцию
postsNewQuery :: Int -> Params Int -> SQL.Query
postsNewQuery page tagParams = template [sql|
        SELECT * FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
                WHERE posts.id IN ({0}) {1}
        |] [postsSubquery tagParams, pageQuery page] where 

        postsSubquery :: Params Int -> SQL.Query
        postsSubquery (ParamsAll tagIds) = [sql|SELECT posts.id FROM posts|] `whereAll` map (\tagId -> postTagsSubquery [tagId]) tagIds
        postsSubquery (ParamsIn tagIds) = [sql|SELECT posts.id FROM posts|] `whereAll` [postTagsSubquery tagIds]
        postsSubquery ParamsEmpty = [sql|SELECT posts.id FROM posts|]


        -- postTagSubquery :: Int -> SQL.Query
        -- postTagSubquery tagId = template [sql| EXISTS (
        --         SELECT 1 FROM contents
        --                 LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        --                 WHERE contents.id = posts.content_id
        --                 AND tags_to_contents.tag_id = {0})
        --         |] [q tagId]

        postTagsSubquery :: [Int] -> SQL.Query
        postTagsSubquery tagIds = exists $
                [sql| SELECT 1 FROM contents
                        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                        WHERE contents.id = posts.content_id
                        AND tags_to_contents.tag_id
                |] `inList` tagIds


--localhost/posts?tag=1
-- SELECT * FROM posts 
--         LEFT JOIN contents ON contents.id = posts.content_id 
--         LEFT JOIN authors ON authors.id = contents.author_id LEFT
--         JOIN users ON users.id = authors.user_id 
--         LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id 
--         LEFT JOIN tags ON tags.id = tags_to_contents.tag_id WHERE posts.id IN 
--         (SELECT posts.id FROM posts WHERE EXISTS 
--                 ( SELECT 1 FROM contents 
--                 LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id 
--                 WHERE contents.id = posts.content_id 
--                 AND tags_to_contents.tag_id) IN (1)) 
-- LIMIT 20 OFFSET 0


--определям post_ids постов, в которых есть данный тег
--сюда передавать все данныe для формирования запроса???
-- postIdsQuery :: SQL.Query
-- postIdsQuery = [sql|
--         SELECT DISTINCT posts.id FROM posts
--             LEFT JOIN contents ON contents.id = posts.content_id
--             LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
--             LEFT JOIN tags ON tags.id = tags_to_contents.tag_id --эту таблицу можно убрать
--         |]


--------------------ALL -??

--проверить в редакторе, также ALL!!
-- exist = [sql|
--         SELECT DISTINCT posts.id
--                 FROM posts WHERE
--                 EXISTS (
--                         SELECT posts.id FROM posts
--                         LEFT JOIN contents ON contents.id = posts.content_id
--                         LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id   
--                         WHERE tags_to_contents.tag_id  = 1 )
--                 -- AND
--                 -- EXISTS (
--                 --         SELECT posts.id FROM posts
--                 --         LEFT JOIN contents ON contents.id = posts.content_id
--                 --         LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id   
--                 --         WHERE tags_to_contents.tag_id  = 2 )
--                 -- EXISTS (
--                 --         SELECT posts.id FROM posts
--                 --         LEFT JOIN contents ON contents.id = posts.content_id
--                 --         LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id   
--                 --         WHERE tags_to_contents.tag_id  = 3 )
--         |]

-- limit = [sql|
--         SELECT * FROM tags
--         LIMIT 3 OFFSET 2
--         |]
        

-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 

tagsQuery :: SQL.Query
tagsQuery = [sql|SELECT * FROM tags|]

-------------------------Pagination------------------------------------------------------
pageQuery :: Int -> SQL.Query
pageQuery page = template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
        quantity = 20






