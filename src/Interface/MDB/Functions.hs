module Interface.MDB.Functions where

import Common.Convert (Convert (..))
import Common.Functions (Template (template), (<$$>))
import Data.Int (Int64)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Class (MDB (..), MTrans)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log

-----------------------------Query functions-----------------------------------
-- As a rule, queries in this module should be correct, since all queries are checked at the logic level.
-- However, in case of an error, detailed information about the error is logged, and the user is given a standard text.
-- Ideally, we can give the user an error code or sequence number,
-- but this is outside the scope of the educational task.

ehandler :: (Error.MError m, Log.MLog m) => Query -> Error.Error -> m a
ehandler query0 err = do
  Log.errorM "An error occurred in the request:"
  Log.errorM $ show query0
  Log.errorM $ show err
  Error.throw Error.dbErrorDefault

-- | Query that returns a value
query :: (MDB m, Show r, FromRow r) => Query -> m [r]
query query0 = do
  Log.debugM query0
  -- LiftIO cannot be used because error handling is lost
  Error.catch (dbquery query0) $ ehandler query0

-- | Query that returns the number of changes without automatic recording the number of changed entities
execute :: MDB m => Query -> [Query] -> m Int64
execute query0 queries = do
  let query1 = template query0 queries
  Error.catch (dbexecute query0) $ ehandler query1

-- | Query without automatic recording of the number of changed entities
execute_ :: MDB m => Query -> [Query] -> m ()
execute_ query0 queries = do
  n <- execute query0 queries
  Log.debugM $ template "Выполнен запрос, изменено {0} строк" [show n]

-- | Helper function for automatic recording of the number of changed entities
_execute :: MTrans m => Cache.QueryType -> Cache.APIType -> Query -> [Query] -> m ()
_execute queryType apiType query0 queries = do
  let query1 = template query0 queries
  Log.debugM query1
  rows <- Error.catch (dbexecute query1) $ ehandler query1
  Cache.addChanged queryType apiType rows

-- | Insert query with automatic recording of the number of changed entities
insert :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
insert = _execute Cache.Insert

-- | Update query with automatic recording of the number of changed entities
update :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
update = _execute Cache.Update

-- | Delete query with automatic recording of the number of changed entities
delete :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
delete = _execute Cache.Delete

-----------------------------Templates-----------------------------------------
-- Various templates for composing complex queries from simple pieces

whereAll :: Query -> [Query] -> Query
whereAll query0 conditions = concat2 [sql|WHERE|] query0 $ concatWithAnd conditions

whereAllM :: Error.MError m => Query -> [m Query] -> m Query
whereAllM query0 mconditions = do
  (return . concat2 [sql|WHERE|] query0 . concatWithAnd) <$$> mconditions

concatWithAnd :: [Query] -> Query
concatWithAnd = concatWith [sql|AND|]

concatWith :: Query -> [Query] -> Query
concatWith _ [] = mempty
concatWith _ [x] = x
concatWith splitter (x : xs) = concat2 splitter x (concatWith splitter xs)

concat2 :: Query -> Query -> Query -> Query
concat2 _ query1 query2 | query1 == mempty = query2
concat2 _ query1 query2 | query2 == mempty = query1
concat2 splitter query1 query2 = query1 <+> splitter <+> query2

(<+>) :: Query -> Query -> Query
(<+>) = concat2 " "

(<<+>>) :: Error.MError m => m Query -> m Query -> m Query
(<<+>>) mquery1 mquery2 = (<+>) <$> mquery1 <*> mquery2

inList :: Convert a => Query -> [a] -> Query
inList _ [] = "FALSE"
inList field values = template [sql|{0} IN ({1})|] [field, concatWith "," $ map toQuery values]

inSubquery :: Query -> Query -> Query
inSubquery field subquery = template [sql|{0} IN ({1})|] [field, subquery]

inSubqueryM :: Error.MError m => Query -> m Query -> m Query
inSubqueryM field subquery = return . template [sql|{0} IN ({1})|] <$$> [return field, subquery]

exists :: Query -> Query
exists query0 = template [sql|EXISTS ({0})|] [query0]

brackets :: Query -> Query
brackets query0 = template [sql|({0})|] [query0]

list :: [Query] -> Query
list queries = template [sql|({0})|] [concatWith "," queries]

toQuery :: Convert a => a -> Query
toQuery = Query . convert
