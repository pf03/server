module Interface.MCache.Functions where

import Common.Types (BSName)
import Data.Char (toLower)
import Data.Int (Int64)
import qualified Data.Map as M
import Interface.MCache.Class (MCache (..))
import Interface.MCache.Types
  ( API (..),
    APIType (Id),
    Auth (Unauthorized),
    Cache (..),
    Changed (..),
    Param (ParamEq),
    ParamValue (IntParam, StringParam),
    ParamsMap,
    QueryType (..),
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
addChanged queryType apiType n = do
  let apiTypeChanged = M.fromList [(apiTypeHelper apiType, n)]
  let queryTypeChanged = M.fromList [(queryTypeHelper queryType, apiTypeChanged)]
  let newChange = Changed queryTypeChanged
  modifyCache $ \cache -> cache {cacheChanged = cacheChanged cache <> newChange}
  where
    apiTypeHelper :: APIType -> String
    apiTypeHelper = plural . lower . show
      where
        plural :: String -> String
        plural str = case str of
          "category" -> "categories"
          _ -> str <> "s"
        lower (x : xs) = toLower x : xs
        lower [] = []

    queryTypeHelper :: QueryType -> String
    queryTypeHelper Insert = "created"
    queryTypeHelper Update = "edited"
    queryTypeHelper Delete = "deleted"
    queryTypeHelper Upload = "uploaded"
    queryTypeHelper Select = "selected"
    queryTypeHelper SelectById = "selected"
    queryTypeHelper Auth = "authorized"
    queryTypeHelper Load = "loaded"
    queryTypeHelper Empty = "none"

getChanged :: MCache m => m Changed
getChanged = getsCache cacheChanged

resetChanged :: MCache m => m ()
resetChanged = modifyCache $ \cache -> cache {cacheChanged = mempty}

setParams :: MCache m => ParamsMap -> m ()
setParams params = modifyCache $ \cache -> cache {cacheParams = params}

getParams :: MCache m => m ParamsMap
getParams = getsCache cacheParams

modifyParams :: MCache m => (ParamsMap -> ParamsMap) -> m ParamsMap
modifyParams f = do
  params0 <- getParams
  setParams $ f params0
  getParams

modifyParamsM :: MCache m => (ParamsMap -> m ParamsMap) -> m ParamsMap
modifyParamsM f = do
  params <- getParams
  newParams <- f params
  setParams newParams
  getParams

getParam :: MCache m => BSName -> m Param
getParam name = getsCache (\cache -> cacheParams cache M.! name)

-- | For example "user_id", "tag_id"
addIdParam :: MCache m => BSName -> Int -> m ParamsMap
addIdParam name paramId = modifyParams $ M.insert name (ParamEq (IntParam paramId))

addIdParam_ :: MCache m => BSName -> Int -> m ()
addIdParam_ name paramId = do
  _ <- addIdParam name paramId
  return ()

addStrParam :: MCache m => BSName -> String -> m ParamsMap
addStrParam name str = modifyParams $ M.insert name (ParamEq (StringParam str))

addStrParam_ :: MCache m => BSName -> String -> m ()
addStrParam_ name str = do
  _ <- addStrParam name str
  return ()

resetCache :: MCache m => m ()
resetCache = setCache defaultCache

defaultCache :: Cache
defaultCache = Cache {cacheChanged = mempty, cacheAuth = Unauthorized,cacheParams = mempty, cacheApi = API Empty []}

getAuth :: MCache m => m Auth
getAuth = getsCache cacheAuth

setAuth :: MCache m => Auth -> m ()
setAuth auth = modifyCache $ \cache -> cache {cacheAuth = auth}

getAPI :: MCache m => m API
getAPI = getsCache cacheApi

setAPI :: MCache m => API -> m ()
setAPI api = modifyCache $ \state -> state {cacheApi = api}