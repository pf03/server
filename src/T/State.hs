{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
module T.State where

-- Our Modules
import           Interface.Cache            as Cache
import           Interface.Error            as Error
import           Interface.Log              as Log
import           Logic.IO.Config            as Config

-- Other Modules
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Database.PostgreSQL.Simple
import           GHC.Generics               hiding (S)

-- | Данный модуль формирует State для одной из реализаций класса типов MT
-- (и остальных классов) - трансформера T

-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)
data S = S {
    configWarp   :: ConfigWarp,
    connectionDB :: Connection,
    configLog    :: LogConfig,
    logSettings  :: LogSettings,
    cache        :: Cache
} deriving (Show, Generic)

-----------------------------Getters&Setters-----------------------------------
getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> m ()
setLogSettings cs e = modify $ \s -> s {logSettings = Log.LogSettings cs e}

getLogConfig :: MonadState S m => m LogConfig
getLogConfig = gets configLog

-- setLogConfig :: MonadState S m => LogConfig -> m ()
-- setLogConfig lc = modify $ \s -> s {configLog = lc}

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp

getConnection:: MonadState S m => m Connection
getConnection = gets connectionDB


