{-# LANGUAGE DeriveGeneric #-}
module Logic.IO.Config where

-- Our modules
import           Interface.Error            as Error
import           Interface.Log              as Log

-- Other modules
import           Control.Exception          (IOException)
import           Data.Aeson
import qualified Data.ByteString.Lazy       as L
import           Database.PostgreSQL.Simple
import           GHC.Generics               (Generic)
import           Network.Wai.Handler.Warp   as Warp (Port)
import           System.IO.Error            (isDoesNotExistError)

-----------------------------Types---------------------------------------------
data Config = Config {
    _warp :: ConfigWarp,
    _db   :: DBConnectInfo,
    _log  :: LogConfig
} deriving (Show, Generic)

newtype DBConnectInfo = DBConnectInfo {getConnectInfo :: ConnectInfo}

instance Show DBConnectInfo where
    show (DBConnectInfo ci) = show ci

instance FromJSON DBConnectInfo where
    parseJSON  = withObject "object" $ \o -> do
        ci <- ConnectInfo
            <$> o .: "connectHost"
            <*> o .: "connectPort"
            <*> o .: "connectUser"
            <*> o .: "connectPassword"
            <*> o .: "connectDatabase"
        return $ DBConnectInfo ci

newtype ConfigWarp = ConfigWarp{
    warpPort:: Port
} deriving (Show, Generic)

instance FromJSON ConfigWarp

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 1 }

-----------------------------Functions-----------------------------------------
-- | Read config as both object and string
readConfig :: MIOError m => m (Config, String)
readConfig = do
    bs <- L.readFile pathConfig `Error.catchEIO` handler
    fileConfig <- eDecode bs
    return (fileConfig, show bs) where
        handler :: IOException -> E
        handler e
            | isDoesNotExistError e = ConfigError "Configuration file not found!"
            | otherwise = ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"

