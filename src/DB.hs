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


