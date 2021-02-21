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


--сожет сделать тут все -таки кортежи из Row-типов??
-------------------------Post-------------------------------------------------------------
-- data Post = Post {
--     post :: Row.Post,
--     content :: Row.Content,
--     author :: Row.Author,
--     user :: Row.User,
--     tag :: Maybe Row.Tag
-- } deriving (Show, Generic)
-- instance ToJSON Post

type Post = Row.Post :. Row.Content :. Row.Author :. Row.User :. (Maybe Row.TagToContent) :. Maybe (Row.Tag)

-- instance FromRow Post where
--     fromRow = do
--         post <- fromRow
--         content <- fromRow
--         author <- fromRow
--         user <- fromRow
--         _  <- fromRow :: RowParser (Maybe Row.TagToContent) --tags_to_contents, not user
--         tag <- fromRow
--         return $ Post {..}

postQuery :: SQL.Query
postQuery = [sql|
        SELECT * FROM posts
            LEFT JOIN contents ON contents.id = posts.content_id
            LEFT JOIN authors ON authors.id = contents.author_id
            LEFT JOIN users ON users.id = authors.user_id
            LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
            LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
        |]
----------------------------Category-----------------------------------------------------------

type Category = Row.Category 
categoryQuery = [sql|SELECT * FROM categories|]

-------------------------------Author---------------------------------------------------------

-- data Author = Author{
--     author :: Row.Author,
--     user :: Row.User
-- }

--проверить, будет ли это работать???
--type Author = (Row.Author, Row.User)
type Author = Row.Author :. Row.User
type Post666 = Row.Post :. Row.Content :. Select.Author

-- instance FromRow Select.Author where
--     fromRow = do
--         author <- fromRow
--         user <- fromRow
--         return (author :. user)



authorQuery = [sql|SELECT * FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id|]

----------------------------------

