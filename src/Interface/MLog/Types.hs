{-# LANGUAGE DeriveGeneric #-}

module Interface.MLog.Types where

import Common.Functions (deletePrefixOptions)
import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import GHC.Generics (Generic)

data Config = Config
  { configColorEnabled :: Bool,
    configTerminalEnabled :: Bool,
    configFileEnabled :: Bool,
    configMinLevel :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON $ deletePrefixOptions 6

data Level
  = Debug -- Debug data
  | Info -- Information about app work
  | Warn -- Warnings
  | Error -- Non-critical error, that can be given to the user in one form or another
  | Critical -- Critical error leading to application termination
  deriving (Eq, Enum, Ord, Show)

data ColorScheme
  = BlueScheme
  | CyanScheme
  | GreenScheme
  | YellowScheme
  | BlackScheme
  deriving (Show)

data Settings = Settings {colorScheme :: ColorScheme, debugModeEnabled :: Bool} deriving (Show)