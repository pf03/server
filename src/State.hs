{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
module State where
import Types 
--import qualified Data.Map.Internal as M
import Data.Map as M ((!))
import qualified Data.Map as M 
import qualified Log 

import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple
import API
import Data.Int
import Data.Char

--getters && setters lens like
getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs e fn = modify $ \s -> s {logSettings = Log.LogSettings cs e fn}

getLogConfig :: MonadState S m => m Log.ConfigLog
getLogConfig = gets configLog

-- getConfigWarp :: MonadState S m => m ConfigWarp
-- getConfigWarp = gets configWarp --trivial

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp --trivial

getConnection:: MonadState S m => m Connection
getConnection = gets connectionDB --trivial

--более элегантное решение через моноид
addChanged :: MonadState S m => QueryType -> APIType -> Int64 -> m ()
addChanged _ _ n |n <= 0 = return ();
addChanged _ (Id _) _ = return ();
addChanged Select _ _ = return ();
addChanged SelectById _ _ = return ();
addChanged qt at n = do
    let newChange = Changed $ M.fromList [(queryType qt, M.fromList [(apiTypes at, n)])]
    modify $ \st -> st {changed = changed st <> newChange} where

    apiTypes :: APIType -> String 
    apiTypes = plural . lower . show where
        plural :: String -> String
        plural str = case str of
            "category" -> "categories"
            _ -> str <> "s"
        lower (x:xs) = toLower x : xs

    queryType :: QueryType -> String 
    queryType Insert = "created"
    queryType Update = "edited"
    queryType Delete = "deleted"
    queryType Upload = "uploaded"


getChanged :: MonadState S m => m Changed 
getChanged = gets changed

resetChanged :: MonadState S m => m ()
resetChanged = modify $ \st -> st {changed = mempty}

setParams :: MonadState S m => ParamsMap Param -> m () 
setParams params = modify $ \s-> s{params = params}

getParams :: MonadState S m => m (ParamsMap Param)
getParams = gets params

modifyParams :: MonadState S m => (ParamsMap Param -> ParamsMap Param) -> m (ParamsMap Param)
modifyParams f = do 
    params <- getParams
    setParams $ f params
    getParams


getParam :: MonadState S m => BSName -> m Param
getParam name = gets (\st -> params st ! name)

--добавить user_id, author_id и т. д.
addIdParam :: MonadState S m => BSName -> Int -> m (ParamsMap Param)
addIdParam name pid = modifyParams $ M.insert name (ParamEq (Int pid))

addStrParam :: MonadState S m => BSName -> String -> m (ParamsMap Param)
addStrParam name str = modifyParams $ M.insert name (ParamEq (Str str))

resetState :: MonadState S m => m ()
resetState = modify $ \st -> st {changed = mempty, params = mempty}

getAuth :: MonadState S m => m Auth 
getAuth = gets auth

setAuth :: MonadState S m => Auth -> m () 
setAuth auth = modify $ \s-> s{auth = auth}

instance Semigroup Changed where
    (<>) = mappend

instance Monoid Changed where 
    mempty = Changed mempty 
    mappend (Changed map1) (Changed map2) = Changed $ M.unionWith (M.unionWith (+)) map1 map2