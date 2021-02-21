module Query --(Query.query_,)
where
import Database.PostgreSQL.Simple as SQL
import Types
import Class
import qualified State as S
import Data.Int
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import Common

query_ :: FromRow r => Query -> T [r]
query_ q = do
    conn <- S.getConnection
    toT $ SQL.query_ conn q

query :: (ToRow q, FromRow r) => Query -> q -> T [r]
query query q = do
    conn <- S.getConnection
    toT $ SQL.query conn query q

executeMany :: ToRow q => Query -> [q] -> T Int64
executeMany q list = do 
    conn <- S.getConnection
    toT $ SQL.executeMany conn q list

execute_ :: Query -> T Int64
execute_ q = do
    conn <- S.getConnection
    toT $ SQL.execute_ conn q

execute__ :: Query -> T ()
execute__ q = do
    Query.execute_ q
    return()

-- q :: Convert a => a -> SQL.Query 
-- q = Query . convert

--мне кажется, что это уже все есть, я просто не умею пользоваться
whereAll :: Query -> [Query] -> Query
whereAll q conditions = Query.concat2 Query.where_ q $ Query.all conditions

all :: [Query] -> Query
all = Query.concat Query.and

concat :: Query -> [Query] -> Query
concat splitter [] = mempty
concat splitter (x:[]) = x
concat splitter (x:xs) = concat2 splitter x (Query.concat splitter xs)

concat2 :: Query -> Query -> Query -> Query
concat2 splitter q1 q2|q1 == mempty = q2
concat2 splitter q1 q2|q2 == mempty  = q1
concat2 splitter q1 q2 = q1 <+> splitter <+> q2

and :: Query
and = [sql|AND|]

space :: Query 
-- space = [sql| |]
space = " "


where_ :: Query
where_ = [sql|WHERE|]

(<+>) :: SQL.Query -> SQL.Query -> SQL.Query
--(<+>) = concat2 space --cyclic dpendency
(<+>) q1 q2|q1 == mempty = q2
(<+>) q1 q2|q2 == mempty  = q1
(<+>) q1 q2 = q1 <> space <> q2

test1 :: Query
test1 = "id=1"
test2 :: Query
test2 = "tag=2"

q :: Convert a => a -> SQL.Query 
q = Query . convert



-- where_sdsd :: Query
-- where_sdsd = Query "dfgdf"