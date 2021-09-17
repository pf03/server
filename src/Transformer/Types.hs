module Transformer.Types where

import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Control.Monad.State.Lazy (MonadIO, MonadState, StateT (..), gets, modify, liftIO)
import Control.Monad.Trans.Except (catchE, throwE)
import qualified Database.PostgreSQL.Simple as SQL
import GHC.Generics (Generic)
import Interface.Class (MCache, MDB, MError, MIOError, MLog, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config

-----------------------------Types---------------------------------------------
newtype Transformer a = Transformer {getTransformer :: StateT State (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState State)

data State = State
  { configWarp :: Config.ConfigWarp,
    dBConnectInfo :: Config.DBConnectInfo,
    configLog :: Log.Config,
    logSettings :: Log.Settings,
    cache :: Cache.Cache
  }
  deriving (Show, Generic)

-----------------------------Instances-----------------------------------------
instance MLog Transformer where
  getSettings = Transformer $ do
    gets logSettings
  setSettings colorScheme logEnable = Transformer $ do
    modify $ \state -> state {logSettings = Log.Settings colorScheme logEnable}
  getConfig = Transformer $ do
    gets configLog
  message logConfig0 logSettings0 logLevel0 str = Transformer $ do
    Log.writeMessageIO logConfig0 logSettings0 logLevel0 str

instance MError Transformer where
  throwServerError :: Error.Error -> Transformer a
  throwServerError err = Transformer $ do
    lift $ throwE err
  catchServerError :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catchServerError ta f = Transformer $ do
    StateT $ \state -> catchE (runStateT (getTransformer ta) state) $ \err ->
      runStateT (getTransformer $ f err) state

instance MIOError Transformer

instance MDB Transformer where
  mdbQuery query = do
    Config.DBConnectInfo connectInfo <- Transformer $ gets dBConnectInfo
    connection <- liftIO $ SQL.connect connectInfo
    result <- Error.liftEIO $ SQL.query_ connection query
    liftIO $ SQL.close connection
    return result
  mdbExecute query = do
    Config.DBConnectInfo connectInfo <- Transformer $ gets dBConnectInfo
    connection <- liftIO $ SQL.connect connectInfo
    result <- Error.liftEIO $ SQL.execute_ connection query
    liftIO $ SQL.close connection
    return result

--error handling (with bracket function) and logging will be fixed in next commits..
-- runConnection :: (MIOError m) => Config.Config -> m Connection
-- runConnection config = do
--   let logConfig0 = Config.configLog config
--   connection <- Error.catchServerError (connectDB . Config.getConnectInfo . Config.configDb $ config) $ \err -> do
--     Log.writeCritical logConfig0 logSettings0 "Error DB connection while start the transformer: "
--     Log.writeCritical logConfig0 logSettings0 $ show err
--     Error.throwServerError err
--   Log.writeInfo logConfig0 logSettings0 "DB connected successfully..."
--   return connection

-- -----------------------------DB------------------------------------------------
-- connectDB :: MIOError m => ConnectInfo -> m Connection
-- connectDB connectInfo = connect connectInfo `Error.catchEIO` handler
--   where
--     handler :: SqlError -> Error.Error
--     handler _ = Error.DBError "Error DB Connection!"

instance MCache Transformer where
  getCache = Transformer $ gets cache
  setCache cache0 = Transformer $ modify (\state -> state {cache = cache0})

instance MTrans Transformer