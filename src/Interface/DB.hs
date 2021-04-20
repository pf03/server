module Interface.DB where

-- Our Modules
import           Common.Misc
import           Interface.Cache                  as Cache
import           Interface.Error                  as Error
import           Interface.Log                    as Log

-- Other Modules
import           Data.Int
import           Database.PostgreSQL.Simple       hiding (execute, execute_, query_)
import qualified Database.PostgreSQL.Simple       as SQL (execute_, query_)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types

-----------------------------Class---------------------------------------------
class (Log.MLog m, MIOError m) => MDB m where
    getConnection :: m Connection

class (MDB m, MCache m) => MT m

connectDB :: MIOError m => ConnectInfo -> m Connection
connectDB connectInfo = connect connectInfo `Error.catchEIO` handler where
    handler :: SqlError -> E
    handler _ = DBError "Ошибка соединения с базой данных!"

-----------------------------Query functions-----------------------------------
-- | Как правило, в этом запросы должны быть корректные, так как все запросы проверяются на уровне логики.
-- Однако в случае ошибки, логгируется подробная информация об ошибке, а пользователю отдается стандартный текст.
-- В идеале пользователю можно отдать пользователю код ошибки или порядковый номер,
-- но это выходит за рамки учебного задания.

-- | Запрос, возвращающий значение
-- LiftIO использовать нельзя, т.к теряется обработка ошибок
query :: (MDB m,  Show r, FromRow r) => Query -> m [r]
query qu = do
    Log.debugM qu
    conn <- getConnection
    Error.catch (liftEIO $ SQL.query_ conn qu ) $ \e -> do
        Log.errorM "Произошла ошибка в запросе:"
        Log.errorM $ show qu
        Log.errorM $ show e
        Error.throw dbErrorDefault

-- | Запрос, возвращающий количество изменений, без автоматической записи количества изменных сущностей
execute :: MDB m => Query -> [Query] -> m Int64
execute q0 qs = do
    let qu = template q0 qs
    conn <- getConnection
    Error.catch (liftEIO $ SQL.execute_ conn qu) $ \e -> do
        Log.errorM "Произошла ошибка в запросе:"
        Log.errorM $ show qu
        Log.errorM $ show e
        Error.throw dbErrorDefault

-- | Запрос без автоматической записи количества изменных сущностей
execute_ :: MDB m => Query -> [Query] -> m ()
execute_ q0 qs = do
    n <- execute q0 qs
    Log.debugM $ template "Выполнен запрос, изменено {0} строк" [show n] --и это, т .е результат выполнения

-- | Вспомогательная функция для автоматической записи количества изменный сущностей
_execute :: MT m => QueryType -> APIType -> Query -> [Query] ->  m ()
_execute queryType apiType q0 qs   = do
    let qu = template q0 qs
    Log.debugM qu
    conn <- getConnection
    rows <- liftEIO $ SQL.execute_ conn qu
    Cache.addChanged queryType apiType rows

-- | Запросы с автоматической записью измененных сущностей
insert  :: MT m => APIType -> Query -> [Query] ->  m ()
insert = _execute Insert

update  :: MT m => APIType -> Query -> [Query] ->  m ()
update = _execute Update

delete  :: MT m => APIType -> Query -> [Query] ->  m ()
delete = _execute Delete

-----------------------------Templates-----------------------------------------
-- Разнообразные шаблоны для составления сложных запросов из простых кусков

whereAll :: Query -> [Query] -> Query
whereAll qu conditions = Interface.DB.concat2 Interface.DB.where_ qu $ Interface.DB.all conditions

whereAllM :: MError m => Query -> [m Query] -> m Query
whereAllM qu mconditions = do
    (return . Interface.DB.concat2 Interface.DB.where_ qu . Interface.DB.all) <$$> mconditions

any :: [Query] -> Query
any = Interface.DB.concat [sql|OR|]

all :: [Query] -> Query
all = Interface.DB.concat Interface.DB.and

concat :: Query -> [Query] -> Query
concat _ []     = mempty
concat _ [x]    = x
concat splitter (x:xs) = concat2 splitter x (Interface.DB.concat splitter xs)

concat2 :: Query -> Query -> Query -> Query
concat2 _ q1 q2|q1 == mempty     = q2
concat2 _ q1 q2|q2 == mempty     = q1
concat2 splitter q1 q2                  = q1 <+> splitter <+> q2

and :: Query
and = [sql|AND|]

space :: Query
space = " "

where_ :: Query
where_ = [sql|WHERE|]

(<+>) :: Query -> Query -> Query
(<+>) q1 q2|q1 == mempty    = q2
(<+>) q1 q2|q2 == mempty    = q1
(<+>) q1 q2                 = q1 <> space <> q2

(<<+>>) :: MError m => m Query -> m Query -> m Query
(<<+>>) mq1 mq2 = (<+>) <$> mq1 <*> mq2

inList :: Convert a => Query -> [a] -> Query
inList _ [] = "FALSE"
inList field values = template [sql|{0} IN ({1})|] [field, Interface.DB.concat "," $ map q values]

inSubquery :: Query -> Query -> Query
inSubquery field subquery  = template [sql|{0} IN ({1})|] [field, subquery]

inSubqueryM :: MError m => Query -> m Query -> m Query
inSubqueryM field subquery  = return . template [sql|{0} IN ({1})|] <$$> [return field, subquery]

exists :: Query -> Query
exists qu = template [sql|EXISTS ({0})|] [qu]

brackets :: Query -> Query
brackets qu = template [sql|({0})|] [qu]

list :: [Query] -> Query
list qs = template [sql|({0})|] [Interface.DB.concat "," qs]

q :: Convert a => a -> Query
q = Query . convert
