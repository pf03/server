module Cache where

import Types 
--import qualified Data.Map.Internal as M
import Data.Map as M ((!))
import qualified Data.Map as M 
import API
import Data.Int
import Data.Char

addChanged :: MCache m => QueryType -> APIType -> Int64 -> m ()
addChanged _ _ n |n <= 0 = return ();
addChanged _ (Id _) _ = return ();
addChanged Select _ _ = return ();
addChanged SelectById _ _ = return ();
addChanged qt at n = do
    let newChange = Changed $ M.fromList [(queryType qt, M.fromList [(apiTypes at, n)])]
    modifyCache $ \cache -> cache {changed = changed cache <> newChange} where

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


getChanged :: MCache m => m Changed 
getChanged = getsCache changed

resetChanged :: MCache m => m ()
resetChanged = modifyCache $ \st -> st {changed = mempty}

setParams :: MCache m => ParamsMap Param -> m () 
setParams params = modifyCache $ \s-> s{params = params}

getParams :: MCache m => m (ParamsMap Param)
getParams = getsCache params 

modifyParams :: MCache m => (ParamsMap Param -> ParamsMap Param) -> m (ParamsMap Param)
modifyParams f = do 
    params <- getParams
    setParams $ f params
    getParams


getParam :: MCache m => BSName -> m Param
getParam name = getsCache (\st -> params st ! name)

--добавить user_id, author_id и т. д.
addIdParam :: MCache m => BSName -> Int -> m (ParamsMap Param)
addIdParam name pid = modifyParams $ M.insert name (ParamEq (Int pid))

addStrParam :: MCache m => BSName -> String -> m (ParamsMap Param)
addStrParam name str = modifyParams $ M.insert name (ParamEq (Str str))

resetCache :: MCache m => m ()
resetCache = modifyCache $ \st -> st {changed = mempty, params = mempty}

getAuth :: MCache m => m Auth 
getAuth = getsCache auth

setAuth :: MCache m => Auth -> m () 
setAuth auth = modifyCache $ \s-> s{auth = auth}

instance Semigroup Changed where
    (<>) = mappend

instance Monoid Changed where 
    mempty = Changed mempty 
    mappend (Changed map1) (Changed map2) = Changed $ M.unionWith (M.unionWith (+)) map1 map2