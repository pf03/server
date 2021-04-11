{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


------------------------------------Config---------------------------------------------------------

--в случае разветвления логики сделать отдельный конфиг для каждого приложения
--состояние приложения, используется в трансформере, Reader




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







--type APIName = String



-- type Modified = M.Map String Int64 
--newtype Changed = Changed {fromChanged :: M.Map String (M.Map String Int64) } deriving (Show, Generic)




data Action = Check | Execute --flag





