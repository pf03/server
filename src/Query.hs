module Query --(Query.query_,)
where
import Database.PostgreSQL.Simple as SQL
import Types
import Class
import qualified State as S
import Data.Int
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types hiding (Show (..) )
import Common
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Log
import API 
import Transformer

--Ошибки в этом модуле не должны отдаваться пользователю, а записываться в лог. Пользователю должен отдаваться стандартный текст!!!

--Для единоообразия во все запросы можно встроить template
query_ :: (Show r, FromRow r) => Query -> T [r]
query_ q = do
    conn <- S.getConnection
    Log.debugT q
    toT $ SQL.query_ conn q

query :: (Show r, ToRow q, FromRow r) => Query -> q -> T [r]
query query q = do
    Log.debugT query 
    conn <- S.getConnection
    toT $ SQL.query conn query q

executeMany :: ToRow q => Query -> [q] -> T Int64
executeMany q list = do 
    Log.debugT q 
    conn <- S.getConnection
    toT $ SQL.executeMany conn q list

--без автоматической записи изменений
execute_ :: Query -> [Query] -> T Int64
execute_ q qs = do
    let query = template q qs
    Log.debugT query 
    conn <- S.getConnection
    toT $ SQL.execute_ conn query

_execute :: QueryType -> APIType -> Query -> [Query] ->  T ()
_execute queryType apiType q qs   = do
    let query = template q qs
    Log.debugT query 
    conn <- S.getConnection
    rows <- toT $ SQL.execute_ conn query
    S.addChanged queryType apiType rows
    --S.getChanged

-- _executeM :: QueryType -> APIType -> Query -> [T Query] ->  T ()
-- _executeM queryType apiType q mqs = _execute queryType apiType q <$$> mqs


--новая версия включает в себя template и автоматическую запись в State количество модифицированных строк
insert  :: APIType -> Query -> [Query] ->  T ()
insert = _execute Insert

-- insertM  :: APIType -> Query -> [T Query] ->  T ()
-- insertM = _executeM Insert


update  :: APIType -> Query -> [Query] ->  T ()
update = _execute Update

delete  :: APIType -> Query -> [Query] ->  T ()
delete = _execute Delete

-- execute__ :: Query -> T ()
-- execute__ q = do
--     n <- Query.execute_ q []
--     Log.textT Log.Debug $ template "Выполнен запрос, изменено {0} строк" [show n] --и это, т .е результат выполнения
--     return()

execute__ :: Query -> [Query] -> T ()
execute__ q qs = do
    n <- Query.execute_ q qs 
    Log.textT Log.Debug $ template "Выполнен запрос, изменено {0} строк" [show n] --и это, т .е результат выполнения
    --return()

whereAll :: Query -> [Query] -> Query
whereAll q conditions = Query.concat2 Query.where_ q $ Query.all conditions

whereAllM :: MError m => Query -> [m Query] -> m Query
whereAllM q mconditions = do 
    (return . Query.concat2 Query.where_ q . Query.all) <$$> mconditions

any :: [Query] -> Query
any = Query.concat [sql|OR|]

all :: [Query] -> Query
all = Query.concat Query.and

concat :: Query -> [Query] -> Query
concat splitter [] = mempty
concat splitter [x] = x
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



--showQ1 = show . T.unpack . T.decodeUtf8 . fromQuery

where_ :: Query
where_ = [sql|WHERE|]

(<+>) :: SQL.Query -> SQL.Query -> SQL.Query
--(<+>) = concat2 space --cyclic dpendency
(<+>) q1 q2|q1 == mempty = q2
(<+>) q1 q2|q2 == mempty  = q1
(<+>) q1 q2 = q1 <> space <> q2

(<<+>>) :: MError m => m SQL.Query -> m SQL.Query -> m SQL.Query
(<<+>>) mq1 mq2 = (<+>) <$> mq1 <*> mq2

--select * from users where first_name in ('Anna', 'Boris', 'Carla')

inList :: Convert a => Query -> [a] -> Query
inList field [] = "FALSE"
inList field values = template [sql|{0} IN ({1})|] [field, Query.concat "," $ map q values] 

inSubquery :: Query -> Query -> Query
inSubquery field subquery  = template [sql|{0} IN ({1})|] [field, subquery]

inSubqueryM :: MError m => Query -> m Query -> m Query
inSubqueryM field subquery  = return . template [sql|{0} IN ({1})|] <$$> [return field, subquery] 

exists :: Query -> Query
exists q = template [sql|EXISTS ({0})|] [q]

brackets :: Query -> Query
brackets q = template [sql|({0})|] [q]

list :: [Query] -> Query
list qs = template [sql|({0})|] [Query.concat "," qs] 

emp = Query.query_ [sql|SELECT posts.id FROM posts WHERE FALSE|] ::T [Only Int]  --or TRUE

--не очень нравится
-- (<->) :: Convert a => a -> [Query] -> [Query]
-- (<->) value list = (q value) : list
-- infixr 1 <->

-- (<-->) :: Convert a, Convert b => a -> b -> [Query]
-- (<-->) value1 value2 = [q value1, q value2]
-- infixr 2 <-->

-- que :: Query
-- que = template [sql|{2}.id BETWEEN {0} AND {1}|] $ (page-1)*20+1 <-> page*20 <--> tname  where
--     page= 7::Int;
--     tname ="table"::String

test1 :: Query
test1 = "id=1"
test2 :: Query
test2 = "tag=2"

q :: Convert a => a -> SQL.Query 
q = Query . convert



-- where_sdsd :: Query
-- where_sdsd = Query "dfgdf"