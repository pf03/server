{-# LANGUAGE DeriveGeneric #-}
module Logic.IO.Config where

-- Our Modules
import           Interface.Error            as Error
import           Interface.Log              as Log

-- Other Modules
import           Control.Exception          (IOException)
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import           Database.PostgreSQL.Simple
import           GHC.Generics               (Generic)
import           System.IO.Error            (isDoesNotExistError)
import Network.Wai.Handler.Warp as Warp ( Port )

-----------------------------Types---------------------------------------------
data Config = Config {
    _warp :: ConfigWarp,
    _db   :: ConnectInfo,
    _log  :: LogConfig
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

-----------------------------Functions-----------------------------------------
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

