module Interface.MLog.Class where

import Interface.MLog.Types
  ( ColorScheme,
    Config,
    Level,
    Settings,
  )

class Monad m => MLog m where
  getSettings :: m Settings
  setSettings :: ColorScheme -> Bool -> m ()
  getConfig :: m Config
  message :: Config -> Settings -> Level -> String -> m ()