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
addChanged queryType apiType n = do
  let newChange = Changed $ M.fromList [(queryTypeHelper queryType, M.fromList [(apiTypeHelper apiType, n)])]
  modifyCache $ \cache -> cache {changed = changed cache <> newChange}
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
getChanged = getsCache changed

resetChanged :: MCache m => m ()
resetChanged = modifyCache $ \cache -> cache {changed = mempty}

setParams :: MCache m => ParamsMap -> m ()
setParams params0 = modifyCache $ \cache -> cache {params = params0}

getParams :: MCache m => m ParamsMap
getParams = getsCache params

modifyParams :: MCache m => (ParamsMap -> ParamsMap) -> m ParamsMap
modifyParams f = do
  params0 <- getParams
  setParams $ f params0
  getParams

modifyParamsM :: MCache m => (ParamsMap -> m ParamsMap) -> m ParamsMap
modifyParamsM f = do
  params0 <- getParams
  newParams <- f params0
  setParams newParams
  getParams

getParam :: MCache m => BSName -> m Param
getParam name = getsCache (\cache -> params cache M.! name)

-- | For example "user_id", "tag_id"
addIdParam :: MCache m => BSName -> Int -> m ParamsMap
addIdParam name paramId = modifyParams $ M.insert name (ParamEq (Int paramId))

addIdParam_ :: MCache m => BSName -> Int -> m ()
addIdParam_ name paramId = do
  _ <- addIdParam name paramId
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
defaultCache = Cache {changed = mempty, auth = Unauthorized, params = mempty, api = API Empty []}

getAuth :: MCache m => m Auth
getAuth = getsCache auth

setAuth :: MCache m => Auth -> m ()
setAuth auth0 = modifyCache $ \cache -> cache {auth = auth0}

getAPI :: MCache m => m API
getAPI = getsCache api

setAPI :: MCache m => API -> m ()
setAPI api0 = modifyCache $ \state -> state {api = api0}