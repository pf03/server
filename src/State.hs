{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
module State where
import Types 
--import qualified Data.Map.Internal as M
import Data.Map as M ((!))
import qualified Data.Map as M 
import qualified Log 

import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple
import API
import Data.Int
import Data.Char

--этот модуль в конечном итоге удалить!!!

--getters && setters lens like
getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs e fn = modify $ \s -> s {logSettings = Log.LogSettings cs e fn}

getLogConfig :: MonadState S m => m Log.ConfigLog
getLogConfig = gets configLog

-- getConfigWarp :: MonadState S m => m ConfigWarp
-- getConfigWarp = gets configWarp --trivial

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp --trivial

getConnection:: MonadState S m => m Connection
getConnection = gets connectionDB --trivial


