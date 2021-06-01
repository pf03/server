module Transformer.Transformer where

import Control.Monad.Except (ExceptT, MonadIO, runExceptT, void)
import Control.Monad.State.Lazy (StateT (runStateT))
import Database.PostgreSQL.Simple (ConnectInfo, Connection, SqlError, connect)
import Interface.Class (MIOError)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config
import qualified System.Console.ANSI as Color
import Transformer.State as S (ConnectionDB (ConnectionDB), State (..), Transformer)

-----------------------------External------------------------------------------

-- | Run and show result of transformer
showT :: Show a => Transformer a -> IO ()
showT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  value <- getValue config connection m
  showValue config value

-- | Run transformer without showing
runT :: Transformer a -> IO ()
runT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  _ <- getValue config connection m
  return ()

-- | Evaluate value of transformer with default value in error case
evalT :: Transformer a -> a -> Config.Config -> IO a
evalT m def config = runE def $ do
  connection <- runConnection config
  getValue config connection m

-- | Evaluate value of transformer with error handler
evalTwithHandler :: Transformer a -> (Error.Error -> a) -> Config.Config -> IO a
evalTwithHandler m handler config = runEwithHandler handler $ do
  connection <- runConnection config
  getValue config connection m

-----------------------------Internal------------------------------------------
runConfig :: (MIOError m) => m Config.Config
runConfig = do
  let ls = Log.Settings Color.Cyan True
  config <- Error.catch Config.readConfig $ \err -> do
    Log.critical Log.defaultConfig ls "Error config read while run the transfomer:"
    Log.critical Log.defaultConfig ls $ show err
    Error.throw err
  let lc = Config.log config
  Log.info lc ls "Config read successfully..."
  return config

runConnection :: (MIOError m) => Config.Config -> m Connection
runConnection config = do
  let ls = Log.Settings Color.Cyan True
  let lc = Config.log config
  connection <- Error.catch (connectDB . Config.getConnectInfo . Config.db $ config) $ \e -> do
    Log.critical lc ls "Error DB connection while start the transformer: "
    Log.critical lc ls $ show e
    Error.throw e
  Log.info lc ls "DB connected successfully..."
  return connection

getValue :: Config.Config -> Connection -> Transformer a -> ExceptT Error.Error IO a
getValue config connection m = do
  let s = getS config connection
  let ls = Log.Settings Color.Cyan True
  let lc = Config.log config
  a <- Error.catch (runStateT m s) $ \e -> do
    Log.error lc ls "Application error: "
    Log.error lc ls $ show e
    Error.throw e
  return $ fst a

showValue :: (MonadIO m, Show a) => Config.Config -> a -> m ()
showValue config value = do
  let ls = Log.Settings Color.Cyan True
  let lc = Config.log config
  Log.info lc ls "Result: "
  Log.info lc ls $ show value
  return ()

-----------------------------ExceptT E IO a------------------------------------

-- | Run ExceptT transformer without error handling with default value
runE :: a -> ExceptT Error.Error IO a -> IO a
runE a m = do
  eb <- runExceptT m
  case eb of
    Left _ -> return a
    Right b -> return b

-- | Run ExceptT transformer with error handling
runEwithHandler :: (Error.Error -> a) -> ExceptT Error.Error IO a -> IO a
runEwithHandler handler m = do
  eb <- runExceptT m
  case eb of
    Left e -> return $ handler e
    Right b -> return b

-- | Value doesn't matter
runE_ :: ExceptT Error.Error IO () -> IO ()
runE_ m = void (runExceptT m)

exceptToMaybe :: ExceptT Error.Error IO a -> IO (Maybe a)
exceptToMaybe m = do
  ea <- runExceptT m
  case ea of
    Left _ -> return Nothing
    Right a -> return $ Just a

-----------------------------Config--------------------------------------------
getS :: Config.Config -> Connection -> State
getS Config.Config {Config.warp = cw, Config.db = _, Config.log = cl} connection =
  State
    { configWarp = cw,
      connectionDB = ConnectionDB connection,
      configLog = cl,
      logSettings = Log.defaultSettings,
      cache = Cache.defaultCache
    }

-----------------------------DB------------------------------------------------
connectDB :: MIOError m => ConnectInfo -> m Connection
connectDB connectInfo = connect connectInfo `Error.catchEIO` handler
  where
    handler :: SqlError -> Error.Error
    handler _ = Error.DBError "Error DB Connection!"
