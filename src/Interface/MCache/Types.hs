module Interface.MCache.Types where

import Common.Types (BSName)
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import qualified Data.Map as M
import Database.PostgreSQL.Simple.Time (Date)
import GHC.Generics (Generic)

data Cache = Cache
  { changed :: Changed,
    auth :: Auth,
    params :: ParamsMap,
    api :: API
  }
  deriving (Show, Generic)

newtype Changed = Changed (M.Map String (M.Map String Int64)) deriving (Show, Generic)

instance ToJSON Changed

instance Semigroup Changed where
  (<>) = mappend

instance Monoid Changed where
  mempty = Changed mempty
  mappend (Changed map1) (Changed map2) = Changed $ M.unionWith (M.unionWith (+)) map1 map2

data Auth = Unauthorized | Authorized Int | Admin Int deriving (Show, Eq)

type ParamsMap = M.Map BSName Param

data Param
  = ParamEq {paramEq :: Val}
  | ParamIn [Val]
  | ParamAll [Val]
  | ParamLt Val
  | ParamGt Val
  | ParamBt (Val, Val)
  | ParamLike Val
  | ParamNull
  | ParamNo
  deriving (Show, Eq)

data Val = Str {valStr :: String} | Int {valInt :: Int} | Date {valDate :: Date} deriving (Show, Eq)

data API = API QueryType [APIType] deriving (Show)

data QueryType = Select | SelectById | Insert | Update | Delete | Upload | Load | Auth | Empty deriving (Show, Read, Eq)

data APIType = Post | User | Author | Category | Tag | Draft | Comment | Photo | Content | Id Int | Image String deriving (Show, Read, Eq)