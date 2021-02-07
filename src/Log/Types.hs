{-# LANGUAGE DeriveGeneric #-}
module Log.Types where 
import GHC.Generics hiding (S)
import System.Console.ANSI
import Data.Aeson
import Data.Aeson.Types

data ConfigLog = ConfigLog{
    colorEnable :: Enable,
    terminalEnable :: Enable,
    fileEnable :: Enable,
    minLevel :: Int  --уровень включения логов, в файле удобней указывать Int, а не LogLevel
} deriving (Show, Generic)

instance FromJSON ConfigLog
instance ToJSON ConfigLog

data LogLevel =  Debug | Data | Info | Warning | Error  deriving (Eq, Enum, Ord, Show)
type ColorScheme = Color
type Enable= Bool
type FuncName = String
data LogSettings = LogSettings {colorScheme:: ColorScheme, logEnable :: Enable, funcName :: FuncName} deriving Show
