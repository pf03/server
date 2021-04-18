module Interface.DB --(Query.query_,)
where
import           Common.Misc
import           Control.Monad.IO.Class
import qualified Data.ByteString                  as B
import           Data.Int
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Database.PostgreSQL.Simple       as SQL
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types hiding (Show (..))
import           Interface.Cache                  as Cache
import           Interface.Error                  as Error
import           Interface.Log                    as Log

--это для postgreSQL, но можно абстрагироваться еще сильнее по типу connection
-- class Monad m => MDB m where
--     getConnection :: m Connection  --setConnection не нужно, соединение устанавливается еще до формирования монады

--DB по умолчанию уже использует интерфейс лога и обработки ошибок
class (Log.MLog m, MIOError m) => MDB m where
    getConnection :: m Connection
--полный трансформер
class (MDB m, MCache m) => MT m

connectDB :: MIOError m => ConnectInfo -> m Connection
connectDB connectInfo = connect connectInfo `Error.catchEIO` handler where
    handler :: SqlError -> E
    handler e = DBError "Ошибка соединения с базой данных!"

--Ошибки в этом модуле не должны отдаваться пользователю, а записываться в лог. Пользователю должен отдаваться стандартный текст!!!
--Для единоообразия во все запросы можно встроить template

query_ :: (MDB m,  Show r, FromRow r) => Query -> m [r]
query_ q = do
    conn <- Interface.DB.getConnection
    Log.debugM q
    liftEIO $ SQL.query_ conn q --LiftIO использовать нельзя, т.к теряется обработка ошибок

-- query_ :: (Show r, FromRow r) => Query -> T [r]
-- query_ q = do
--     conn <- S.getConnection
--     Log.debugM q
--     toT $ SQL.query_ conn q

query :: (MDB m, Show r, ToRow q, FromRow r) => Query -> q -> m[r]
query query q = do
    Log.debugM query
    conn <- Interface.DB.getConnection
    liftEIO $ SQL.query conn query q

executeMany :: (MDB m, ToRow q) => Query -> [q] -> m Int64
executeMany q list = do
    Log.debugM q
    conn <- Interface.DB.getConnection
    liftEIO $ SQL.executeMany conn q list

--без автоматической записи изменений
execute_ :: MDB m => Query -> [Query] -> m Int64
execute_ q qs = do
    let query = template q qs
    Log.debugM query
    conn <- Interface.DB.getConnection
    liftEIO $ SQL.execute_ conn query

_execute :: MT m => QueryType -> APIType -> Query -> [Query] ->  m ()
_execute queryType apiType q qs   = do
    let query = template q qs
    Log.debugM query
    conn <- Interface.DB.getConnection
    rows <- liftEIO $ SQL.execute_ conn query
    Cache.addChanged queryType apiType rows
    --S.getChanged

-- _executeM :: QueryType -> APIType -> Query -> [T Query] ->  T ()
-- _executeM queryType apiType q mqs = _execute queryType apiType q <$$> mqs


--новая версия включает в себя template и автоматическую запись в State количество модифицированных строк
insert  :: MT m => APIType -> Query -> [Query] ->  m ()
insert = _execute Insert

-- insertM  :: APIType -> Query -> [T Query] ->  T ()
-- insertM = _executeM Insert


update  :: MT m => APIType -> Query -> [Query] ->  m ()
update = _execute Update

delete  :: MT m => APIType -> Query -> [Query] ->  m ()
delete = _execute Delete

-- execute__ :: Query -> T ()
-- execute__ q = do
--     n <- Query.execute_ q []
--     Log.textT Log.Debug $ template "Выполнен запрос, изменено {0} строк" [show n] --и это, т .е результат выполнения
--     return()

execute__ :: MDB m => Query -> [Query] -> m ()
execute__ q qs = do
    n <- Interface.DB.execute_ q qs
    Log.debugM $ template "Выполнен запрос, изменено {0} строк" [show n] --и это, т .е результат выполнения
    --return()

whereAll :: Query -> [Query] -> Query
whereAll q conditions = Interface.DB.concat2 Interface.DB.where_ q $ Interface.DB.all conditions

whereAllM :: MError m => Query -> [m Query] -> m Query
whereAllM q mconditions = do
    (return . Interface.DB.concat2 Interface.DB.where_ q . Interface.DB.all) <$$> mconditions

any :: [Query] -> Query
any = Interface.DB.concat [sql|OR|]

all :: [Query] -> Query
all = Interface.DB.concat Interface.DB.and

concat :: Query -> [Query] -> Query
concat splitter []     = mempty
concat splitter [x]    = x
concat splitter (x:xs) = concat2 splitter x (Interface.DB.concat splitter xs)

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
inList field values = template [sql|{0} IN ({1})|] [field, Interface.DB.concat "," $ map q values]

inSubquery :: Query -> Query -> Query
inSubquery field subquery  = template [sql|{0} IN ({1})|] [field, subquery]

inSubqueryM :: MError m => Query -> m Query -> m Query
inSubqueryM field subquery  = return . template [sql|{0} IN ({1})|] <$$> [return field, subquery]

exists :: Query -> Query
exists q = template [sql|EXISTS ({0})|] [q]

brackets :: Query -> Query
brackets q = template [sql|({0})|] [q]

list :: [Query] -> Query
list qs = template [sql|({0})|] [Interface.DB.concat "," qs]

--emp = Interface.DB.query_ [sql|SELECT posts.id FROM posts WHERE FALSE|] ::T [Only Int]  --or TRUE

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
