{-# LANGUAGE DeriveGeneric #-}
module Log.Types where 
import GHC.Generics hiding (S)
import System.Console.ANSI

data ConfigLog = ConfigLog{
    colorEnable :: Enable,
    terminalEnable :: Enable,
    fileEnable :: Enable,
    level :: LogLevel  --уровень включения логов
} deriving (Show, Generic)

data LogLevel =  Debug | Data | Info | Warning | Error  deriving (Eq, Enum, Ord, Show)
type ColorScheme = Color
type Enable= Bool
type FuncName = String
data LogSettings = LogSettings {colorScheme:: ColorScheme, logEnable :: Enable, funcName :: FuncName}
