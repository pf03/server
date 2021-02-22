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
import Types (Path)
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ

----------------------------------User-----------------------------------------------------------
type User = Row.User 
usersQuery = [sql|SELECT * FROM users|]

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

-------------------------Tag-------------------------------------------------------------
type Tag = Row.Tag 

tagsQuery :: SQL.Query
tagsQuery = [sql|SELECT * FROM tags|]







