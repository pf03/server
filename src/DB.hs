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

getPosts :: T [User]
getPosts = do
    Query.query_ [sql|SELECT * FROM users|]

getAuthors :: T [Author]
getAuthors = do
    Query.query_ [sql|SELECT * FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id|]

-- getAuthorsUsers :: T [(Author, User)]
-- getAuthorsUsers = do
--     Query.query_ [sql|SELECT * FROM authors
--         LEFT JOIN users
--         ON authors.user_id = users.id|]



