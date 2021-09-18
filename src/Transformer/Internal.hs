module Transformer.Internal where

import Control.Monad.Except (ExceptT, MonadIO, runExceptT, void)
import Control.Monad.State.Lazy (StateT (runStateT))
import Database.PostgreSQL.Simple (ConnectInfo, Connection, SqlError, connect)
import Interface.Class (MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config
import Transformer.Types
  ( ConnectionDB (ConnectionDB),
    ServerState (..),
    ServerStateIO (getServerStateIO),
  )

-----------------------------Run------------------------------------------
runConfig :: (MIOError m) => m Config.Config
runConfig = do
  config <- Error.catchServerError Config.readConfig $ \err -> do
    Log.writeCritical Log.defaultConfig logSettings0 "Error config read while run the transformer:"
    Log.writeCritical Log.defaultConfig logSettings0 $ show err
    Error.throwServerError err
  let logConfig0 = Config.configLog config
  Log.writeInfo logConfig0 logSettings0 "Config read successfully..."
  return config

runConnection :: (MIOError m) => Config.Config -> m Connection
runConnection config = do
  let logConfig0 = Config.configLog config
  connection <- Error.catchServerError (connectDB . Config.getConnectInfo . Config.configDb $ config) $ \err -> do
    Log.writeCritical logConfig0 logSettings0 "Error DB connection while start the transformer: "
    Log.writeCritical logConfig0 logSettings0 $ show err
    Error.throwServerError err
  Log.writeInfo logConfig0 logSettings0 "DB connected successfully..."
  return connection

getValue :: Config.Config -> Connection -> ServerStateIO a -> ExceptT Error.Error IO a
getValue config connection m = do
  let state = configToState config connection
  let logConfig0 = Config.configLog config
  (a, _) <- Error.catchServerError (runStateT (getServerStateIO m) state) $ \err -> do
    Log.writeError logConfig0 logSettings0 "Application error: "
    Log.writeError logConfig0 logSettings0 $ show err
    Error.throwServerError err
  return a

showValue :: (MonadIO m, Show a) => Config.Config -> a -> m ()
showValue config value = do
  let logConfig0 = Config.configLog config
  Log.writeInfo logConfig0 logSettings0 "Result: "
  Log.writeInfo logConfig0 logSettings0 $ show value
  return ()

logSettings0 :: Log.Settings
logSettings0 = Log.Settings Log.CyanScheme True

-----------------------------Config--------------------------------------------
configToState :: Config.Config -> Connection -> ServerState
configToState Config.Config {Config.configWarp = configWarp0, Config.configDb = _, Config.configLog = logConfig0} connection =
  ServerState
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
  eValue <- runExceptT m
  case eValue of
    Left _ -> return defaultValue
    Right value -> return value

-- | Run ExceptT transformer with error handling
runEWithHandler :: (Error.Error -> a) -> ExceptT Error.Error IO a -> IO a
runEWithHandler handler m = do
  eValue <- runExceptT m
  case eValue of
    Left err -> return $ handler err
    Right value -> return value

-- | Value doesn't matter
runE_ :: ExceptT Error.Error IO () -> IO ()
runE_ m = void (runExceptT m)

exceptToMaybe :: ExceptT Error.Error IO a -> IO (Maybe a)
exceptToMaybe m = do
  eValue <- runExceptT m
  case eValue of
    Left _ -> return Nothing
    Right value -> return $ Just value