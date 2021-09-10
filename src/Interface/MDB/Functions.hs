module Interface.MDB.Functions where

import Common.Template (Template (template), templateM)
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

eHandler :: (Error.MError m, Log.MLog m) => Query -> Error.Error -> m a
eHandler query0 err = do
  Log.writeErrorM "An error occurred in the request:"
  Log.writeErrorM $ show query0
  Log.writeErrorM $ show err
  Error.throwServerError Error.dbErrorDefault

-- | Query that returns a value
query_ :: (MDB m, Show r, FromRow r) => Query -> m [r]
query_ query0 = do
  Log.writeDebugM query0
  -- LiftIO cannot be used because error handling is lost
  Error.catchServerError (dbQuery query0) $ eHandler query0

query :: (MDB m, Show r, FromRow r) => Query -> [Query] -> m [r]
query query0 queries = do
  let query1 = template query0 queries
  query_ query1

queryM :: (MDB m, Show r, FromRow r) => Query -> [m Query] -> m [r]
queryM query0 queries = do
  query1 <- templateM query0 queries
  query_ query1

-- | Query that returns the number of changes without automatic recording the number of changed entities
execute :: MDB m => Query -> [Query] -> m Int64
execute query0 queries = do
  let query1 = template query0 queries
  Log.writeDebugM query1
  Error.catchServerError (dbExecute query1) $ eHandler query1

executeM :: MDB m => Query -> [m Query] -> m Int64
executeM query0 queries = do
  query1 <- templateM query0 queries
  Log.writeDebugM query1
  Error.catchServerError (dbExecute query1) $ eHandler query1

-- | Query without automatic recording of the number of changed entities
execute_ :: MDB m => Query -> [Query] -> m ()
execute_ query0 queries = do
  n <- execute query0 queries
  Log.writeDebugM $ template "Query executed, {0} rows changed" [show n]

executeM_ :: MDB m => Query -> [m Query] -> m ()
executeM_ query0 queries = do
  n <- executeM query0 queries
  Log.writeDebugM $ template "Query executed, {0} rows changed" [show n]

-- | Helper function for automatic recording of the number of changed entities
_execute :: MTrans m => Cache.QueryType -> Cache.APIType -> Query -> [Query] -> m ()
_execute queryType apiType query0 queries = do
  let query1 = template query0 queries
  Log.writeDebugM query1
  rows <- Error.catchServerError (dbExecute query1) $ eHandler query1
  Cache.addChanged queryType apiType rows

_executeM :: MTrans m => Cache.QueryType -> Cache.APIType -> Query -> [m Query] -> m ()
_executeM queryType apiType query0 queries = do
  query1 <- templateM query0 queries
  Log.writeDebugM query1
  rows <- Error.catchServerError (dbExecute query1) $ eHandler query1
  Cache.addChanged queryType apiType rows

-- | Insert query with automatic recording of the number of changed entities
insert :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
insert = _execute Cache.Insert

insertM :: MTrans m => Cache.APIType -> Query -> [m Query] -> m ()
insertM = _executeM Cache.Insert

-- | Update query with automatic recording of the number of changed entities
update :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
update = _execute Cache.Update

updateM :: MTrans m => Cache.APIType -> Query -> [m Query] -> m ()
updateM = _executeM Cache.Update

-- | Delete query with automatic recording of the number of changed entities
delete :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
delete = _execute Cache.Delete

deleteM :: MTrans m => Cache.APIType -> Query -> [m Query] -> m ()
deleteM = _executeM Cache.Delete