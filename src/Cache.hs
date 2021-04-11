{-# LANGUAGE DeriveGeneric #-}

module Cache where

--import Types 
--import qualified Data.Map.Internal as M
import Data.Map as M ((!))
import qualified Data.Map as M 
import Data.Int
import Data.Char
import Data.Aeson
import GHC.Generics
import Data.ByteString.Char8 as BC (ByteString)
import Database.PostgreSQL.Simple.Time

-- | Данный модуль реализует класс типов MCache для работы с состоянием в чистом коде.
-- А также соответствующие типы и функции для работы с Cache 

-----------------------------Types---------------------------------------------
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

--Params--
type ParamsMap = M.Map BSName

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

type BSName = BS    --created_at
type BS = ByteString

--API--
data API = API QueryType [APIType] deriving (Show)
data QueryType = Select | SelectById | Insert | Update | Delete | Upload | Auth deriving (Show, Read, Eq)
data APIType = Post | User | Author | Category | Tag | Draft | Comment | Photo | Content | Id Int deriving (Show, Read, Eq)


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