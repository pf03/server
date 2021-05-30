module Interface.MDB.Functions where

import Common.Functions (Template (template))
import Data.Int (Int64)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.Types (Query)
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Class (MDB (..), MTrans)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log

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