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
postsNewQuery :: Int -> Params Int  -> Params Int -> Maybe String -> SQL.Query
postsNewQuery page tagParams categoryParams text = res where
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

        conditions :: [SQL.Query]
        conditions = [
                postIdsSubquery tagParams,
                categoriesCond categoryParams,
                textCond text
                ]

        postIdsSubquery :: Params Int -> SQL.Query
        postIdsSubquery (ParamsAll tagIds) = [sql|posts.id|] `inSubquery` 
                ([sql|SELECT posts.id FROM posts|] `whereAll` map (\tagId -> postTagsSubquery [tagId]) tagIds)
        postIdsSubquery (ParamsIn tagIds) = [sql|posts.id|] `inSubquery`
                ([sql|SELECT posts.id FROM posts|] `whereAll` [postTagsSubquery tagIds])
        postIdsSubquery ParamsAny = [sql|TRUE|]

        postTagsSubquery :: [Int] -> SQL.Query
        postTagsSubquery tagIds = exists $
                [sql| SELECT 1 FROM contents
                        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                        WHERE contents.id = posts.content_id
                        AND tags_to_contents.tag_id
                |] `inList` tagIds
        
        categoriesCond :: Params Int -> SQL.Query
        categoriesCond ParamsAny = [sql|TRUE|]
        categoriesCond (ParamsIn categoryIds) = [sql|contents.category_id|] `inList` categoryIds
        categoriesCond (ParamsAll categoryIds) =  error "this pattern should not occur!" 

        textCond :: Maybe String -> SQL.Query
        textCond Nothing = [sql|TRUE|]
        textCond (Just text) = template [sql|contents.text ILIKE '%{0}%'|] [q text]


        


-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 

tagsQuery :: SQL.Query
tagsQuery = [sql|SELECT * FROM tags|]

-------------------------Pagination------------------------------------------------------
pagination :: Int -> SQL.Query
pagination page = template [sql|LIMIT {0} OFFSET {1}|] [q quantity, q $ (page-1)*quantity] where
        quantity = 20






