{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

--importPriority = 100
module Types where
import qualified Data.Map.Internal as M
import Control.Exception
--mtl
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Except

import qualified Log
import GHC.Generics hiding (S)
import Data.Aeson
import Data.Aeson.Types
import System.Console.ANSI
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.Wai.Internal ( Request, Response, ResponseReceived )
import Database.PostgreSQL.Simple.Time
import Data.Text (pack, Text(..))
import qualified Data.ByteString as BC
import qualified Data.Map as M
import Data.Int

--Этот модуль содержит все типы в проекте, исключая специфические для каждого из приложений VK, Telegram
--Специфические нужно разместить в VK.Types и Telegram.Types

--------------------------------------Main--------------------------------------------------------
type TimeOut = Int  --таймаут для long polling

--------------------------------------App----------------------------------------------------------
data App = VK | Telegram deriving (Show, Generic)
instance FromJSON App
instance ToJSON App

--------------------------------------Error--------------------------------------------------------
data E = ParseError String | RequestError String | ConfigError String | DBError String | IOError String | AuthError String | DevError String | SomeError String
-- это надо убрать, раз мы пользуемся мондами Parser - Except - ExceptT
type ES = Either String
type EE = Either E

----------------------------------------Transformer------------------------------------------------
--основной трансформер, контейнер с одним параметром
type T = StateT S (ExceptT E IO)

------------------------------------Config---------------------------------------------------------

--в случае разветвления логики сделать отдельный конфиг для каждого приложения
--состояние приложения, используется в трансформере, Reader
data S = S {
    configWarp :: ConfigWarp,
    -- configDB :: ConnectInfo,
    connectionDB :: Connection,
    configLog :: Log.ConfigLog,
    logSettings :: Log.LogSettings,
    changed :: Changed,
    auth :: Auth,
    params :: ParamsMap Param
    
} deriving (Show, Generic)


data Cache = Cache {getCache :: String}

data Config = Config {
    _warp :: ConfigWarp,
    _db :: ConnectInfo,
    _log :: Log.ConfigLog
} deriving (Show, Generic)

newtype ConfigWarp = ConfigWarp{
    warpPort:: Port
} deriving (Show, Generic)

-- data ConfigDB = ConfigDB{
--     connectHost :: Host, 
--     connectPort :: Int, 
--     connectUser :: String, 
--     connectPassword :: String,
--     connectDatabase :: String
-- } deriving (Show, Generic)
instance Show Connection where 
    show c = "connected"

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 1 } 

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

instance FromJSON ConfigWarp
instance ToJSON ConfigWarp
instance FromJSON ConnectInfo
instance ToJSON ConnectInfo

---------------------------------------API-------------------------------------------------------------

-- data API a  = API {
--     funcName :: FuncName,
--     pathInfo :: PathInfo,
--     handler :: T [a]
-- }

--------------------------------Parse && Logic----------------------------------------------------------
--type Token = String
type Host = String
type Path = String
type Key = String -- для удобства парсинга
type UserId = Int
type ChatId = Int
type UpdateId = Int
type IntId = Int --более общее, чтобы не плодить сущности
type StrId = String
type FileId = String
type Message = String  --текстовое сообщение
data Command = Help | Repeat | Start | Unknown String| Button Int deriving (Show, Eq)  --возможные команды боту
type Caption = String  --описание картинки или видео
type Label = String  --подписи на кнопках
type Url = String
type ItemName = String
type UserName = String
type StateChanged = Bool
type Port = Int
type PathInfo = [Text]
type FuncName = String
----------------------------------WAI-------------------------------------------------------------------------
-- type ApplicationT = Request -> (Response -> T ResponseReceived) -> T ResponseReceived


----------------------------------DB------------------------------------------------------------------------------

--параметры строки запроса, например tags_in, tags_all. tag=3 соответствует tags_in = [3], хотя и tags_all= [3] подходит
--data Params a = ParamsIn [a] | ParamsAll [a]  | ParamsLT a | ParamsGT a | ParamsLike a | ParamsAny deriving (Show)
-- data Templ = Eq | In | All | Lt | Gt | Like  deriving (Show, Eq)  
-- data Param = Param Templ Val | ParamAny deriving Show
-- data Val = Str String| Int Int | List [Int] | Date Date deriving Show


--новая версия
data Templ = Eq | In | All | Lt | Gt | Bt | Like  deriving (Show, Eq)  
data Param = ParamEq {paramEq :: Val} | ParamIn [Val] | ParamAll [Val] | ParamLt Val | ParamGt Val | ParamBt (Val, Val) | ParamLike Val | ParamNull | ParamNo   deriving (Show, Eq)
data Val = Str { valStr :: String} | Int { valInt :: Int} | Date { valDate :: Date} deriving (Show, Eq)
--data Val = Str {getStr :: String} | Int {getInt :: Int} | Date {getDate :: Date} deriving Show

data ParamType = ParamTypePage | ParamTypeStr | ParamTypeInt | ParamTypeDate | ParamTypeSort  [BSName] | ParamTypeFileName [BSName] deriving Show
--data Order a = OrderEq a | OrderLT a | OrderGT a | OrderAny

type BS = BC.ByteString
type BSName = BS  --created_at
type BSKey = BS --created_at__lt
type BSValue = BS --"2021-01-01"
type BSTempl = BS --"__lt"
type ParamsMap = M.Map BSName
--type ParamDesc = [(BSName, [Templ], ParamType)]
--type ParamDesc2 = [(BSName, [Templ], ParamType, Bool)] --последнее - обязательность --использовать адт здесь       

--это потом тоже под мап переделаем
-- data ParamDesc = ParamDesc {
--     bsname :: BSName,
--     templs :: [Templ],
--     paramType :: ParamType,
--     must :: Bool
-- } 

data ParamDesc = ParamDesc {
    templs :: [Templ],
    paramType :: ParamType,
    must :: Bool,
    nullable :: Bool
}      
type APIName = String

data Auth = AuthNo | AuthUser Int | AuthAdmin Int deriving (Show, Eq)

-- type Modified = M.Map String Int64 
--newtype Changed = Changed {fromChanged :: M.Map String (M.Map String Int64) } deriving (Show, Generic)
newtype Changed = Changed  (M.Map String (M.Map String Int64))  deriving (Show, Generic)

instance ToJSON Changed

data Action = Check | Execute --flag