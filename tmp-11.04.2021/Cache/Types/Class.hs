{-# LANGUAGE DeriveGeneric #-}

module Cache.Types.Class where

class Monad m => MCache m where
    getCache :: m Cache
    setCache :: Cache -> m ()

data Cache = Cache {
    changed :: Changed,
    auth :: Auth,
    params :: ParamsMap Param
} deriving (Show, Generic)

--Changed--
newtype Changed = Changed  (M.Map String (M.Map String Int64))  deriving (Show, Generic)
instance ToJSON Changed

--Auth--
data Auth = AuthNo | AuthUser Int | AuthAdmin Int deriving (Show, Eq)