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

---------------------------------------API-------------------------------------------------------------

data API a  = API {
    funcName :: FuncName,
    pathInfo :: PathInfo,
    handler :: T [a]
}

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
data Param = ParamEq Val | ParamIn [Val] | ParamAll [Val] | ParamLt Val | ParamGt Val | ParamBt (Val, Val) | ParamLike Val | ParamNo  deriving Show
data Val = Str  String | Int Int | Date Date deriving Show
--data Val = Str {getStr :: String} | Int {getInt :: Int} | Date {getDate :: Date} deriving Show

data ParamType = ParamTypePage | ParamTypeStr | ParamTypeInt | ParamTypeDate deriving Show
--data Order a = OrderEq a | OrderLT a | OrderGT a | OrderAny

type BS = BC.ByteString
type BSName = BS  --created_at
type BSKey = BS --created_at__lt
type BSValue = BS --"2021-01-01"
type BSTempl = BS --"__lt"
type ParamDesc = [(BSName, [Templ], ParamType)]
type APIName = String


--  ???
-- constructParam :: Templ -> Val -> Param
-- constructParam Eq val = ParamEq


-- ?? ByteString??  --aeson не работает с ByteString
-- data User = User {
--     userId :: Int,
--     firstName :: String,
--     lastName :: String,
--     avatar :: String,
--     login :: String,
--     pass :: String,
--     userCreationDate :: Date,
--     isAdmin :: Bool
-- } deriving (Show, Generic, FromRow)
-- instance ToJSON User
-- -- instance FromRow User where
-- --     fromRow = User <$> field <*> field

-- data Author = Author {
--     authorId :: Int,
--     user :: User,
--     description :: String
-- } deriving (Show, Generic)

-- instance FromRow Author where
--     --fromRow = Author <$> field <*> field <*> field <*> fromRow <*> field
--     fromRow = do
--         authorId <- field
--         _ <- field :: RowParser Int  --user_id
--         description <- field
--         user <- fromRow 
--         return $ Author authorId user description
-- instance ToJSON Author

-- data Category' = Category'{
--     categoryId' :: Int,
--     parent' :: Maybe Int,
--     categoryName' :: String
-- } deriving (Show, Generic, FromRow)

-- data Category = Category{
--     categoryId :: Int,
--     parent :: Maybe Category,
--     categoryName :: String
-- } deriving (Show, Generic)
-- instance ToJSON Category



-- --это вспомогательный тип, не участвует в API

-- -- type ContentTuple = (Int, User, String, Date, Int, Text, Path)

-- --этот тип нужен потому, что не получается сразу вытянуть рекурсивные категории
-- data Content' = Content' {
--     contentId' :: Int,
--     author' :: Author,
--     contentName' :: String,
--     contentCreationDate' :: Date,
--     category' :: Int,
--     text' :: Text,
--     photo' :: Path,
--     tag :: Maybe Tag
-- } deriving (Show, Generic)
-- instance ToJSON Content'

-- instance FromRow (Maybe Tag) where
--     fromRow = do
--         mtagId <- field
--         mtagName <- field
--         return $ Tag <$> mtagId <*> mtagName


-- instance FromRow Content' where
--     fromRow = do
--         contentId <- field
--         _ <- field :: RowParser Int  --author_id
--         name <- field
--         creationDate <- field
--         categoryId <- field
--         text <- field
--         photo <- field
--         author <- fromRow 
--         _  <- field :: RowParser (Maybe Int) --tags_to_contents
--         _  <- field :: RowParser (Maybe Int)
--         _  <- field :: RowParser (Maybe Int)
--         mtag <- fromRow

--         return $ Content' contentId author name creationDate categoryId text photo mtag

-- data Content = Content {
--     contentId :: Int,
--     author :: Author,
--     contentName :: String,
--     contentCreationDate :: Date,
--     category :: Category,
--     text :: Text,
--     photo :: Path, 
--     tags :: [Tag]
-- } deriving (Show, Generic)
-- instance ToJSON Content



-- -- -- type PostTuple = (Int, Int, User, String, Date, Int, Text, Path)
-- -- instance FromRow PostTuple where
-- --     fromRow = undefined 

-- data Post' = Post' {
--     postId' :: Int,
--     postContent' :: Content'
-- } deriving (Show, Generic)
-- instance ToJSON Post'

-- instance FromRow Post' where
--     fromRow = do
--         postId <- field
--         _ <- field :: RowParser Int  --content_id
--         content <- fromRow
--         return $ Post' postId content


-- data Post = Post {
--     postId :: Int,
--     postContent :: Content
-- } deriving (Show, Generic)
-- instance ToJSON Post

-- data Tag = Tag {
--     tagId :: Int,
--     tagName :: String
-- } deriving (Show, Generic, FromRow)
-- instance ToJSON Tag


-- -- data Draft = Draft {
-- --     draftId :: Int,
-- --     draftContent :: Content
-- -- } deriving (Show, Generic)
-- -- instance ToJSON Draft


-- -- --возможно тут нужен другой инстанс, чтобы хорошо читался фронтендом
-- instance ToJSON Date where
--   toJSON = String . pack . show

--  Вобщем нужно два отдельных типа - один для  бд, другой для json
--может даже третий под конкретный запрос
--UserTable
--UserRow
--UserJSON
--Table.User
--Row.User
--JSON.User
