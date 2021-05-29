module Interface.Class (MLog, MError, MIOError, MCache, MDB, MTrans) where

-- Re-export all classes
import Interface.MDB.Class (MDB, MTrans)
import Interface.MCache.Class (MCache)
import Interface.MError.Class (MError, MIOError)
import Interface.MLog.Class (MLog)