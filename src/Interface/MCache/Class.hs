module Interface.MCache.Class where

import Interface.MCache.Types (Cache)

class Monad m => MCache m where
  getCache :: m Cache
  setCache :: Cache -> m ()