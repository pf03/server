module Query (query_)where
import qualified Database.PostgreSQL.Simple as SQL
import Types
import Class
import qualified State as S

query_ :: SQL.FromRow r => SQL.Query -> T [r]
query_ q = do
    conn <- S.getConnection
    toT $ SQL.query_ conn q

--пощупать версию query!!