{-# LANGUAGE DeriveGeneric #-}

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

--Этот модуль содержит все типы в проекте, исключая специфические для каждого из приложений VK, Telegram
--Специфические нужно разместить в VK.Types и Telegram.Types

--------------------------------------Main--------------------------------------------------------
type TimeOut = Int  --таймаут для long polling

--------------------------------------App----------------------------------------------------------
data App = VK | Telegram deriving (Show, Generic)
instance FromJSON App
instance ToJSON App

--------------------------------------Error--------------------------------------------------------
data E = ParseError String | QueryError String | ConfigError String 
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
    configDB :: ConnectInfo,
    configLog :: Log.ConfigLog,
    logSettings :: Log.LogSettings,
    mconnectionDB :: Maybe Connection
} deriving (Show, Generic)

data Config = Config {
    _warp :: ConfigWarp,
    _db :: ConnectInfo,
    _log :: Log.ConfigLog
} deriving (Show, Generic)

newtype ConfigWarp = ConfigWarp{
    warpPort:: String
} deriving (Show, Generic)

-- data ConfigDB = ConfigDB{
--     connectHost :: Host, 
--     connectPort :: Int, 
--     connectUser :: String, 
--     connectPassword :: String,
--     connectDatabase :: String
-- } deriving (Show, Generic)


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


--------------------------------Parse && Logic----------------------------------------------------------
type Token = String
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



