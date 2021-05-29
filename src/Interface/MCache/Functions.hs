module Interface.MCache.Functions where

import Data.Char (toLower)
import Data.Int (Int64)
import qualified Data.Map as M
import Interface.MCache.Class (MCache (..))
import Interface.MCache.Types
  ( API (..),
    APIType (Id),
    Auth (AuthNo),
    BSName,
    Cache (..),
    Changed (..),
    Param (ParamEq),
    ParamsMap,
    QueryType (..),
    Val (Int, Str),
  )

getsCache :: MCache m => (Cache -> a) -> m a
getsCache f = f <$> getCache

modifyCache :: MCache m => (Cache -> Cache) -> m ()
modifyCache f = do
  cache <- getCache
  setCache $ f cache

-----------------------------Getters&Setters-----------------------------------
addChanged :: MCache m => QueryType -> APIType -> Int64 -> m ()
addChanged _ _ n | n <= 0 = return ()
addChanged _ (Id _) _ = return ()
addChanged Select _ _ = return ()
addChanged SelectById _ _ = return ()
addChanged qt at n = do
  let newChange = Changed $ M.fromList [(queryType qt, M.fromList [(apiTypes at, n)])]
  modifyCache $ \cache -> cache {changed = changed cache <> newChange}
  where
    apiTypes :: APIType -> String
    apiTypes = plural . lower . show
      where
        plural :: String -> String
        plural str = case str of
          "category" -> "categories"
          _ -> str <> "s"
        lower (x : xs) = toLower x : xs
        lower [] = []

    queryType :: QueryType -> String
    queryType Insert = "created"
    queryType Update = "edited"
    queryType Delete = "deleted"
    queryType Upload = "uploaded"
    queryType Select = "selected"
    queryType SelectById = "selected"
    queryType Auth = "authorized"
    queryType Load = "loaded"
    queryType Empty = "none"

getChanged :: MCache m => m Changed
getChanged = getsCache changed

resetChanged :: MCache m => m ()
resetChanged = modifyCache $ \st -> st {changed = mempty}

setParams :: MCache m => ParamsMap -> m ()
setParams p = modifyCache $ \s -> s {params = p}

getParams :: MCache m => m ParamsMap
getParams = getsCache params

modifyParams :: MCache m => (ParamsMap -> ParamsMap) -> m ParamsMap
modifyParams f = do
  p <- getParams
  setParams $ f p
  getParams

modifyParamsM :: MCache m => (ParamsMap -> m ParamsMap) -> m ParamsMap
modifyParamsM f = do
  p <- getParams
  newp <- f p
  setParams newp
  getParams

getParam :: MCache m => BSName -> m Param
getParam name = getsCache (\st -> params st M.! name)

-- | For example "user_id", "tag_id"
addIdParam :: MCache m => BSName -> Int -> m ParamsMap
addIdParam name pid = modifyParams $ M.insert name (ParamEq (Int pid))

addIdParam_ :: MCache m => BSName -> Int -> m ()
addIdParam_ name pid = do
  _ <- addIdParam name pid
  return ()

addStrParam :: MCache m => BSName -> String -> m ParamsMap
addStrParam name str = modifyParams $ M.insert name (ParamEq (Str str))

addStrParam_ :: MCache m => BSName -> String -> m ()
addStrParam_ name str = do
  _ <- addStrParam name str
  return ()

resetCache :: MCache m => m ()
resetCache = setCache defaultCache

defaultCache :: Cache
defaultCache = Cache {changed = mempty, auth = AuthNo, params = mempty, api = API Empty []}

getAuth :: MCache m => m Auth
getAuth = getsCache auth

setAuth :: MCache m => Auth -> m ()
setAuth a = modifyCache $ \s -> s {auth = a}

getAPI :: MCache m => m API
getAPI = getsCache api

setAPI :: MCache m => API -> m ()
setAPI a = modifyCache $ \s -> s {api = a}