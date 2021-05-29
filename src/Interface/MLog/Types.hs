{-# LANGUAGE DeriveGeneric #-}

module Interface.MLog.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Console.ANSI (Color)

data Config = Config
  { colorEnable :: Enable,
    terminalEnable :: Enable,
    fileEnable :: Enable,
    -- | Minimal log level as an integer
    minLevel :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

data Level
  = Debug -- Debug data
  | Info -- Information about app work
  | Warn -- Warnings
  | Error -- Non-critical error, that can be given to the user in one form or another
  | Critical -- Critical error leading to application termination
  deriving (Eq, Enum, Ord, Show)

type ColorScheme = Color

type Enable = Bool

data Settings = Settings {colorScheme :: ColorScheme, debugMode :: Enable} deriving (Show)