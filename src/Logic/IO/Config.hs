{-# LANGUAGE DeriveGeneric #-}

module Logic.IO.Config where

import Control.Exception (IOException)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.ByteString.Lazy as L
import Database.PostgreSQL.Simple (ConnectInfo (ConnectInfo))
import GHC.Generics (Generic)
import Interface.Class (MIOError)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import Network.Wai.Handler.Warp as Warp (Port)
import System.IO.Error (isDoesNotExistError)

-----------------------------Types---------------------------------------------
data Config = Config
  { warp :: ConfigWarp,
    db :: DBConnectInfo,
    log :: Log.Config
  }
  deriving (Show, Generic)

newtype DBConnectInfo = DBConnectInfo {getConnectInfo :: ConnectInfo}

instance Show DBConnectInfo where
  show (DBConnectInfo connectionInfo) = show connectionInfo

instance FromJSON DBConnectInfo where
  parseJSON = withObject "object" $ \object -> do
    connectInfo <-
      ConnectInfo
        <$> object .: "connectHost"
        <*> object .: "connectPort"
        <*> object .: "connectUser"
        <*> object .: "connectPassword"
        <*> object .: "connectDatabase"
    return $ DBConnectInfo connectInfo

newtype ConfigWarp = ConfigWarp
  { warpPort :: Port
  }
  deriving (Show, Generic)

instance FromJSON ConfigWarp

instance FromJSON Config

-----------------------------Functions-----------------------------------------

-- | Read config as both object and string
readConfig :: MIOError m => m Config
readConfig = do
  bs <- L.readFile pathConfig `Error.catchEIO` handler
  Error.eDecode bs
  where
    handler :: IOException -> Error.Error
    handler err
      | isDoesNotExistError err = Error.ConfigError "Configuration file not found!"
      | otherwise = Error.ConfigError "Error reading configuration file"

pathConfig :: FilePath
pathConfig = "config.json"