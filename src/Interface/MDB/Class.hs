module Interface.MDB.Class where

import Data.Int (Int64)
import Database.PostgreSQL.Simple (FromRow, Query)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log

class (Log.MLog m, Error.MIOError m) => MDB m where
  dbQuery :: FromRow r => Query -> m [r]
  dbExecute :: Query -> m Int64

class (MDB m, Cache.MCache m) => MTrans m