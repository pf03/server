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
dbQuery_ :: (MDB m, Show r, FromRow r) => Query -> m [r]
dbQuery_ query0 = do
  Log.writeDebugM query0
  -- LiftIO cannot be used because error handling is lost
  Error.catchServerError (mdbQuery query0) $ eHandler query0

dbQuery :: (MDB m, Show r, FromRow r) => Query -> [Query] -> m [r]
dbQuery query0 queries = do
  let query1 = template query0 queries
  dbQuery_ query1

dbQueryM :: (MDB m, Show r, FromRow r) => Query -> [m Query] -> m [r]
dbQueryM query0 queries = do
  query1 <- templateM query0 queries
  dbQuery_ query1

-- | Query that returns the number of changes without automatic recording the number of changed entities
dbExecute :: MDB m => Query -> [Query] -> m Int64
dbExecute query0 queries = do
  let query1 = template query0 queries
  Log.writeDebugM query1
  Error.catchServerError (mdbExecute query1) $ eHandler query1

dbExecuteM :: MDB m => Query -> [m Query] -> m Int64
dbExecuteM query0 queries = do
  query1 <- templateM query0 queries
  Log.writeDebugM query1
  Error.catchServerError (mdbExecute query1) $ eHandler query1

-- | Query without automatic recording of the number of changed entities
dbExecute_ :: MDB m => Query -> [Query] -> m ()
dbExecute_ query0 queries = do
  n <- dbExecute query0 queries
  Log.writeDebugM $ template "Query executed, {0} rows changed" [show n]

dbExecuteM_ :: MDB m => Query -> [m Query] -> m ()
dbExecuteM_ query0 queries = do
  n <- dbExecuteM query0 queries
  Log.writeDebugM $ template "Query executed, {0} rows changed" [show n]

-- | Helper function for automatic recording of the number of changed entities
_dbExecute :: MTrans m => Cache.QueryType -> Cache.APIType -> Query -> [Query] -> m ()
_dbExecute queryType apiType query0 queries = do
  let query1 = template query0 queries
  Log.writeDebugM query1
  rows <- Error.catchServerError (mdbExecute query1) $ eHandler query1
  Cache.addChanged queryType apiType rows

_dbExecuteM :: MTrans m => Cache.QueryType -> Cache.APIType -> Query -> [m Query] -> m ()
_dbExecuteM queryType apiType query0 queries = do
  query1 <- templateM query0 queries
  Log.writeDebugM query1
  rows <- Error.catchServerError (mdbExecute query1) $ eHandler query1
  Cache.addChanged queryType apiType rows

-- | Insert query with automatic recording of the number of changed entities
dbInsertM :: MTrans m => Cache.APIType -> Query -> [m Query] -> m ()
dbInsertM = _dbExecuteM Cache.Insert

-- | Update query with automatic recording of the number of changed entities
dbUpdate :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
dbUpdate = _dbExecute Cache.Update

dbUpdateM :: MTrans m => Cache.APIType -> Query -> [m Query] -> m ()
dbUpdateM = _dbExecuteM Cache.Update

-- | Delete query with automatic recording of the number of changed entities
dbDelete :: MTrans m => Cache.APIType -> Query -> [Query] -> m ()
dbDelete = _dbExecute Cache.Delete