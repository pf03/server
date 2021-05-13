{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module T.State where

-- Our modules
import           Interface.Cache            as Cache
import           Interface.DB               as DB
import           Interface.Error            as Error
import           Interface.Log              as Log
import           Logic.IO.Config            as Config

-- Other modules
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Database.PostgreSQL.Simple
import           GHC.Generics               hiding (S)
import qualified Database.PostgreSQL.Simple       as SQL (execute_, query_)

-- This module defines State for one of the implementations of the MT type class
-- (and other types classes) - transformer T

-----------------------------Types---------------------------------------------
type T = StateT S (ExceptT E IO)
data S = S {
    configWarp   :: ConfigWarp,
    connectionDB :: ConnectionDB,
    configLog    :: LogConfig,
    logSettings  :: LogSettings,
    cache        :: Cache
} deriving (Show, Generic)

newtype ConnectionDB = ConnectionDB Connection
instance Show ConnectionDB where
    show _ = "connected"

-----------------------------Instances-----------------------------------------
instance Log.MLog T where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
--   setConfig = S.setLogConfig
  message = Log.messageIO

instance MError T where
    throw :: E -> T a
    throw e  = lift $ throwE e

    catch :: T a -> (E -> T a) -> T a
    catch ta f  = StateT $ \s -> catchE (runStateT ta s) $ \e -> runStateT (f e) s

instance MIOError T

instance MCache T where
    getCache = gets cache
    setCache c = modify (\st -> st {cache = c})

instance MDB T where
    dbquery qu = do
      ConnectionDB conn <- gets connectionDB
      liftEIO $ SQL.query_ conn qu 
    dbexecute qu = do
      ConnectionDB conn <- gets connectionDB
      liftEIO $ SQL.execute_ conn qu

instance MT T

-----------------------------Getters&Setters-----------------------------------
getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> m ()
setLogSettings cs e = modify $ \s -> s {logSettings = Log.LogSettings cs e}

getLogConfig :: MonadState S m => m LogConfig
getLogConfig = gets configLog

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp

getConnection:: MonadState S m => m ConnectionDB
getConnection = gets connectionDB