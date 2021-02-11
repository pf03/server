module DB where

import Database.PostgreSQL.Simple.SqlQQ
-- import  qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Log
import Class
import Types
import qualified Query 

getUsers :: T [(Int, String, String)]
getUsers = do
    Query.query_ [sql|SELECT id, login, pass FROM users|]


