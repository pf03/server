module Query --(Query.query_,)
where
import Database.PostgreSQL.Simple as SQL
import Types
import Class
import qualified State as S
import Data.Int

query_ :: FromRow r => Query -> T [r]
query_ q = do
    conn <- S.getConnection
    toT $ SQL.query_ conn q

executeMany :: ToRow q => Query -> [q] -> T Int64
executeMany q list = do 
    conn <- S.getConnection
    toT $ SQL.executeMany conn q list

execute_ :: Query -> T Int64
execute_ q = do
    conn <- S.getConnection
    toT $ SQL.execute_ conn q


-- _constructor :: () -> ()

--пощупать версию query!!