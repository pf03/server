{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Interface.Cache where

import           Data.Aeson
import           Data.ByteString.Char8           as BC (ByteString)
import           Data.Char
import           Data.Int
import           Data.Map                        as M ((!))
import qualified Data.Map                        as M
import           Database.PostgreSQL.Simple.Time
import           GHC.Generics

-- This module implements the MCache typeclass to handle state in pure code.
-- As well as the corresponding types and functions for working with Cache

-----------------------------Types---------------------------------------------
data Cache = Cache {
    changed :: Changed,
    auth    :: Auth,
    params  :: ParamsMap,
    api :: API
} deriving (Show, Generic)

--Changed--
newtype Changed = Changed  (M.Map String (M.Map String Int64))  deriving (Show, Generic)
instance ToJSON Changed

--Auth--
data Auth = AuthNo | AuthUser Int | AuthAdmin Int deriving (Show, Eq)

--Params--
type ParamsMap = M.Map BSName Param

data Param = ParamEq {paramEq :: Val}
    | ParamIn [Val]
    | ParamAll [Val]
    | ParamLt Val
    | ParamGt Val
    | ParamBt (Val, Val)
    | ParamLike Val
    | ParamNull
    | ParamNo   deriving (Show, Eq)

data Val = Str { valStr :: String} | Int { valInt :: Int} | Date { valDate :: Date} deriving (Show, Eq)

type BSName = BS
type BS = ByteString

--API--
data API = API QueryType [APIType] deriving (Show)
data QueryType = Select | SelectById | Insert | Update | Delete | Upload | Load | Auth | Empty deriving (Show, Read, Eq)
data APIType = Post | User | Author | Category | Tag | Draft | Comment | Photo | Content | Id Int | Image String deriving (Show, Read, Eq)

-----------------------------Class---------------------------------------------
class Monad m => MCache m where
    getCache :: m Cache
    setCache :: Cache -> m ()

getsCache :: MCache m => (Cache -> a) -> m a
getsCache f = f <$> getCache

modifyCache :: MCache m => (Cache -> Cache) -> m ()
modifyCache f = do
    cache <- getCache
    setCache $ f cache

-----------------------------Getters&Setters-----------------------------------

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
            _          -> str <> "s"
        lower (x:xs) = toLower x : xs
        lower [] = []

    queryType :: QueryType -> String
    queryType Insert = "created"
    queryType Update = "edited"
    queryType Delete = "deleted"
    queryType Upload = "uploaded"
    queryType Select = "selected"
    queryType SelectById = "selected"
    queryType Auth = "authorized"

getChanged :: MCache m => m Changed
getChanged = getsCache changed

resetChanged :: MCache m => m ()
resetChanged = modifyCache $ \st -> st {changed = mempty}

setParams :: MCache m => ParamsMap -> m ()
setParams p = modifyCache $ \s-> s{params = p}

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
getParam name = getsCache (\st -> params st ! name)

-- | For example "user_id", "tag_id"
addIdParam :: MCache m => BSName -> Int -> m ParamsMap
addIdParam name pid = modifyParams $ M.insert name (ParamEq (Int pid))

addIdParam_ :: MCache m => BSName -> Int -> m ()
addIdParam_ name pid = do 
    _ <- addIdParam name pid
    return ()

addStrParam :: MCache m => BSName -> String -> m ParamsMap
addStrParam name str = modifyParams $ M.insert name (ParamEq (Str str))

resetCache :: MCache m => m ()
resetCache = setCache defaultCache

defaultCache :: Cache
defaultCache = Cache{changed = mempty, auth = AuthNo, params = mempty, api = API Empty []}

getAuth :: MCache m => m Auth
getAuth = getsCache auth

setAuth :: MCache m => Auth -> m ()
setAuth a = modifyCache $ \s-> s{auth = a}

getAPI :: MCache m => m API
getAPI = getsCache api

setAPI :: MCache m => API -> m ()
setAPI a = modifyCache $ \s-> s{api = a}

instance Semigroup Changed where
    (<>) = mappend

instance Monoid Changed where
    mempty = Changed mempty
    mappend (Changed map1) (Changed map2) = Changed $ M.unionWith (M.unionWith (+)) map1 map2
