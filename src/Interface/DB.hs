module Interface.DB where

-- Our Modules
import           Common.Misc
import           Interface.Cache                  as Cache
import           Interface.Error                  as Error
import           Interface.Log                    as Log

-- Other Modules
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (FromRow)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Query (Query))
-- import           Data.Aeson

-----------------------------Class---------------------------------------------
class (Log.MLog m, MIOError m) => MDB m where
    dbquery :: FromRow r => Query -> m [r]
    dbexecute :: Query -> m Int64

-- class (Log.MLog m, MIOError m) => MDB m where
--     getConnection :: m MyConnection

class (MDB m, MCache m) => MT m

-----------------------------Query functions-----------------------------------
-- As a rule, queries in this module should be correct, since all queries are checked at the logic level.
-- However, in case of an error, detailed information about the error is logged, and the user is given a standard text.
-- Ideally, we can give the user an error code or sequence number,
-- but this is outside the scope of the educational task.

ehandler :: (MError m, MLog m) => Query -> E -> m a
ehandler qu e = do
    Log.errorM "An error occurred in the request:"
    Log.errorM $ show qu
    Log.errorM $ show e
    Error.throw dbErrorDefault

-- | Query that returns a value
query :: (MDB m,  Show r, FromRow r) => Query -> m [r]
query qu = do
    Log.debugM qu
    -- LiftIO cannot be used because error handling is lost
    Error.catch (dbquery qu) $ ehandler qu

-- | Query that returns the number of changes without automatic recording the number of changed entities
execute :: MDB m => Query -> [Query] -> m Int64
execute q0 qs = do
    let qu = template q0 qs
    Error.catch (dbexecute qu) $ ehandler qu

-- | Query without automatic recording of the number of changed entities
execute_ :: MDB m => Query -> [Query] -> m ()
execute_ q0 qs = do
    n <- execute q0 qs
    Log.debugM $ template "Выполнен запрос, изменено {0} строк" [show n]

-- | Helper function for automatic recording of the number of changed entities
_execute :: MT m => QueryType -> APIType -> Query -> [Query] ->  m ()
_execute queryType apiType q0 qs   = do
    let qu = template q0 qs
    Log.debugM qu
    rows <- Error.catch (dbexecute qu) $ ehandler qu
    Cache.addChanged queryType apiType rows

-- | Insert query with automatic recording of the number of changed entities
insert  :: MT m => APIType -> Query -> [Query] ->  m ()
insert = _execute Insert

-- | Update query with automatic recording of the number of changed entities
update  :: MT m => APIType -> Query -> [Query] ->  m ()
update = _execute Update

-- | Delete query with automatic recording of the number of changed entities
delete  :: MT m => APIType -> Query -> [Query] ->  m ()
delete = _execute Delete

-----------------------------Templates-----------------------------------------
-- Various templates for composing complex queries from simple pieces

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
concat _ []            = mempty
concat _ [x]           = x
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
