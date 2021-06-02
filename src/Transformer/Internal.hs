{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Transformer.Internal where

import Control.Monad.Except (ExceptT, MonadIO, runExceptT, void)
import Control.Monad.State.Lazy (StateT (runStateT))
import Database.PostgreSQL.Simple (ConnectInfo, Connection, SqlError, connect)
import Interface.Class (MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config
import qualified System.Console.ANSI as Color
import Transformer.Types
  ( ConnectionDB (ConnectionDB),
    State (..),
    Transformer (getTransformer),
  )

-----------------------------Run------------------------------------------
runConfig :: (MIOError m) => m Config.Config
runConfig = do
  config <- Error.catch Config.readConfig $ \err -> do
    Log.critical Log.defaultConfig logSettings0 "Error config read while run the transfomer:"
    Log.critical Log.defaultConfig logSettings0 $ show err
    Error.throw err
  let logConfig0 = Config.log config
  Log.info logConfig0 logSettings0 "Config read successfully..."
  return config

runConnection :: (MIOError m) => Config.Config -> m Connection
runConnection config = do
  let logConfig0 = Config.log config
  connection <- Error.catch (connectDB . Config.getConnectInfo . Config.db $ config) $ \err -> do
    Log.critical logConfig0 logSettings0 "Error DB connection while start the transformer: "
    Log.critical logConfig0 logSettings0 $ show err
    Error.throw err
  Log.info logConfig0 logSettings0 "DB connected successfully..."
  return connection

getValue :: Config.Config -> Connection -> Transformer a -> ExceptT Error.Error IO a
getValue config connection m = do
  let state = configToState config connection
  let logConfig0 = Config.log config
  (a, _) <- Error.catch (runStateT (getTransformer m) state) $ \err -> do
    Log.error logConfig0 logSettings0 "Application error: "
    Log.error logConfig0 logSettings0 $ show err
    Error.throw err
  return a

showValue :: (MonadIO m, Show a) => Config.Config -> a -> m ()
showValue config value = do
  let logConfig0 = Config.log config
  Log.info logConfig0 logSettings0 "Result: "
  Log.info logConfig0 logSettings0 $ show value
  return ()

logSettings0 :: Log.Settings
logSettings0 = Log.Settings Color.Cyan True

-----------------------------Config--------------------------------------------
configToState :: Config.Config -> Connection -> State
configToState Config.Config {Config.warp = configWarp0, Config.db = _, Config.log = logConfig0} connection =
  State
    { configWarp = configWarp0,
      connectionDB = ConnectionDB connection,
      configLog = logConfig0,
      logSettings = Log.defaultSettings,
      cache = Cache.defaultCache
    }

-----------------------------DB------------------------------------------------
connectDB :: MIOError m => ConnectInfo -> m Connection
connectDB connectInfo = connect connectInfo `Error.catchEIO` handler
  where
    handler :: SqlError -> Error.Error
    handler _ = Error.DBError "Error DB Connection!"

-----------------------------ExceptT E IO a------------------------------------

-- | Run ExceptT transformer without error handling with default value
runE :: a -> ExceptT Error.Error IO a -> IO a
runE defaultValue m = do
  evalue <- runExceptT m
  case evalue of
    Left _ -> return defaultValue
    Right value -> return value

-- | Run ExceptT transformer with error handling
runEwithHandler :: (Error.Error -> a) -> ExceptT Error.Error IO a -> IO a
runEwithHandler handler m = do
  evalue <- runExceptT m
  case evalue of
    Left err -> return $ handler err
    Right value -> return value

-- | Value doesn't matter
runE_ :: ExceptT Error.Error IO () -> IO ()
runE_ m = void (runExceptT m)

exceptToMaybe :: ExceptT Error.Error IO a -> IO (Maybe a)
exceptToMaybe m = do
  evalue <- runExceptT m
  case evalue of
    Left _ -> return Nothing
    Right value -> return $ Just value