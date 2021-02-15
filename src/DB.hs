module DB where

import Database.PostgreSQL.Simple.SqlQQ
-- import  qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Database.PostgreSQL.Simple.Time
import Class
import Types
import qualified Query 
import Data.Text


getUsers :: T [User]
getUsers = do
    Query.query_ [sql|SELECT * FROM users|]

getPosts :: T [Post']
getPosts = do
    Query.query_ [sql|SELECT * FROM posts
    LEFT JOIN contents ON contents.id = posts.content_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id|]
    
    --LEFT JOIN categories ON categories.id = contents.category_id
    
getAuthors :: T [Author]
getAuthors = do
    Query.query_ [sql|SELECT * FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id|]

getCategories :: T [(Int, Maybe Int, String)]
getCategories = do 
    Query.query_ [sql|SELECT * FROM categories|]



-- getAuthorsUsers :: T [(Author, User)]
-- getAuthorsUsers = do
--     Query.query_ [sql|SELECT * FROM authors
--         LEFT JOIN users
--         ON authors.user_id = users.id|]



