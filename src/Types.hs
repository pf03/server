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

--Этот модуль содержит все типы в проекте, исключая специфические для каждого из приложений VK, Telegram
--Специфические нужно разместить в VK.Types и Telegram.Types

--------------------------------------Main--------------------------------------------------------
type TimeOut = Int  --таймаут для long polling

--------------------------------------App----------------------------------------------------------
data App = VK | Telegram deriving (Show, Generic)
instance FromJSON App
instance ToJSON App

--------------------------------------Error--------------------------------------------------------
data E = ParseError String | RequestError String | ConfigError String | DBError String | IOError String | SomeError String
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
    logSettings :: Log.LogSettings
    
} deriving (Show, Generic)

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
type Port = Int

----------------------------------WAI-------------------------------------------------------------------------
-- type ApplicationT = Request -> (Response -> T ResponseReceived) -> T ResponseReceived


----------------------------------DB------------------------------------------------------------------------------

-- ?? ByteString??  --aeson не работает с ByteString
data User = User {
    userId :: Int,
    firstName :: String,
    lastName :: String,
    avatar :: String,
    login :: String,
    pass :: String,
    userCreationDate :: Date,
    isAdmin :: Bool
} deriving (Show, Generic, FromRow)
instance ToJSON User
-- instance FromRow User where
--     fromRow = User <$> field <*> field

data Author = Author {
    authorId :: Int,
    user :: User,
    description :: String
} deriving (Show, Generic)
instance ToJSON Author


instance FromRow Author where
    --fromRow = Author <$> field <*> field <*> field <*> fromRow <*> field
    fromRow = do
        authorId <- field
        _ <- field :: RowParser Int  --user_id
        description <- field
        user <- fromRow 
        
        return $ Author authorId user description

-- data Category = Category{
--     categoryId :: Int,
--     parent :: Category
-- } deriving (Show, Generic, FromRow)

-- data Tag = Tag {
--     tagId :: Int,
--     tagName :: Int
-- }


-- data Content = Content {
--     contentId :: Int,
--     author :: User,
--     contentName :: String,
--     contentCreationDate :: Date,
--     category :: Category,
--     text :: Text,
--     photo :: Path 
-- } deriving (Show, Generic, FromRow)
-- data Post = Post {
--     postId :: Int,
--     postContent :: Content
-- } deriving (Show, Generic, FromRow)

-- data Draft = Draft {
--     draftId :: Int,
--     draftContent :: Content
-- } deriving (Show, Generic, FromRow)


-- --возможно тут нужен другой инстанс, чтобы хорошо читался фронтендом
instance ToJSON Date where
  toJSON = String . pack . show


