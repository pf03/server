module Transformer.Internal where

import Control.Monad.Except (ExceptT, MonadIO, runExceptT, void)
import Control.Monad.State.Lazy (StateT (runStateT))
import Interface.Class (MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config
import Transformer.Types
  ( State (..),
    Transformer (getTransformer),
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

getValue :: Config.Config -> Transformer a -> ExceptT Error.Error IO a
getValue config m = do
  let state = configToState config
  let logConfig0 = Config.configLog config
  (a, _) <- Error.catchServerError (runStateT (getTransformer m) state) $ \err -> do
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
configToState :: Config.Config -> State
configToState Config.Config {Config.configWarp = configWarp0, Config.configDb = dBConnectInfo0, Config.configLog = logConfig0} =
  State
    { configWarp = configWarp0,
      dBConnectInfo = dBConnectInfo0,
      configLog = logConfig0,
      logSettings = Log.defaultSettings,
      cache = Cache.defaultCache
    }

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