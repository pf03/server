{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module State where
import Types 
import Data.Map as M ((!))
import qualified Data.Map as M 
import qualified Log 
import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple
import Data.Int
import Data.Char
import Cache
import GHC.Generics hiding (S)
import Control.Monad.Except

-- | Данный модуль формирует State для одной из реализаций класса типов MT 
-- (и остальных классов) - трансформера T

-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)
data S = S {
    configWarp :: ConfigWarp,
    connectionDB :: Connection,
    configLog :: Log.ConfigLog,
    logSettings :: Log.LogSettings,
    cache :: Cache
} deriving (Show, Generic)

-----------------------------Getters&Setters-----------------------------------
getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs e fn = modify $ \s -> s {logSettings = Log.LogSettings cs e fn}

getLogConfig :: MonadState S m => m Log.ConfigLog
getLogConfig = gets configLog

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp

getConnection:: MonadState S m => m Connection
getConnection = gets connectionDB


