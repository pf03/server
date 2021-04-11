{-# LANGUAGE DeriveGeneric #-}
module Config where

import qualified Log
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple
import Error
import Control.Exception
import qualified Data.ByteString.Lazy as L
import System.IO.Error

data Config = Config {
    _warp :: ConfigWarp,
    _db :: ConnectInfo,
    _log :: Log.ConfigLog
} deriving (Show, Generic)

newtype ConfigWarp = ConfigWarp{
    warpPort:: Port
} deriving (Show, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 1 } 

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

instance Show Connection where 
    show c = "connected"

instance FromJSON ConfigWarp
instance ToJSON ConfigWarp
instance FromJSON ConnectInfo
instance ToJSON ConnectInfo
type Port = Int


-- | Read config as both object and string
readConfig :: MIOError m => m (Config, String)
readConfig = do
    bs <- L.readFile pathConfig `Error.catchEIO` handler
    fileConfig <- eDecode bs
    return (fileConfig, show bs) where
        handler :: IOException -> E
        handler e
            | isDoesNotExistError e = ConfigError "Файл конфигурации не найден!"
            | otherwise = ConfigError "Ошибка чтения файла конфигурации"

pathConfig :: FilePath
pathConfig = "config.json"

