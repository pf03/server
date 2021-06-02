{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Transformer.State where

import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Control.Monad.State.Lazy (MonadState, StateT (..), gets, modify)
import Control.Monad.Trans.Except (catchE, throwE)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as SQL (execute_, query_)
import GHC.Generics (Generic)
import Interface.Class (MCache, MDB, MError, MIOError, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config

-- This module defines State for one of the implementations of the MT type class
-- (and other types classes) - transformer T

-----------------------------Types---------------------------------------------
type Transformer = StateT State (ExceptT Error.Error IO)

data State = State
  { configWarp :: Config.ConfigWarp,
    connectionDB :: ConnectionDB,
    configLog :: Log.Config,
    logSettings :: Log.Settings,
    cache :: Cache.Cache
  }
  deriving (Show, Generic)

newtype ConnectionDB = ConnectionDB Connection

instance Show ConnectionDB where
  show _ = "connected"

-----------------------------Instances-----------------------------------------
instance Log.MLog Transformer where
  getSettings = getLogSettings
  setSettings = setLogSettings
  getConfig = getLogConfig
  --   setConfig = S.setLogConfig
  message = Log.messageIO

instance MError Transformer where
  throw :: Error.Error -> Transformer a
  throw err = lift $ throwE err

  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch m f = StateT $ \state -> catchE (runStateT m state) $ \err -> runStateT (f err) state

instance MIOError Transformer

instance MCache Transformer where
  getCache = gets cache
  setCache cache0 = modify (\state -> state {cache = cache0})

instance MDB Transformer where
  dbquery query = do
    ConnectionDB connection <- gets connectionDB
    Error.liftEIO $ SQL.query_ connection query
  dbexecute query = do
    ConnectionDB connection <- gets connectionDB
    Error.liftEIO $ SQL.execute_ connection query

instance MTrans Transformer

-----------------------------Getters&Setters-----------------------------------
getLogSettings :: MonadState State m => m Log.Settings
getLogSettings = gets logSettings

setLogSettings :: MonadState State m => Log.ColorScheme -> Log.Enable -> m ()
setLogSettings colorScheme enable = modify $ \state -> state {logSettings = Log.Settings colorScheme enable}

getLogConfig :: MonadState State m => m Log.Config
getLogConfig = gets configLog

getWarpPort :: MonadState State m => m Config.ConfigWarp
getWarpPort = gets configWarp

getConnection :: MonadState State m => m ConnectionDB
getConnection = gets connectionDB