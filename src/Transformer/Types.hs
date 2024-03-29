module Transformer.Types where

import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Control.Monad.State.Lazy (MonadIO, MonadState, StateT (..), gets, modify)
import Control.Monad.Trans.Except (catchE, throwE)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as SQL (execute_, query_)
import GHC.Generics (Generic)
import Interface.Class (MCache, MDB, MError, MIOError, MLog, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.IO.Config as Config

-----------------------------Types---------------------------------------------
newtype ServerStateIO a = ServerStateIO {getServerStateIO :: StateT ServerState (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState ServerState)

data ServerState = ServerState
  { stateConfigWarp :: Config.ConfigWarp,
    stateConnectionDB :: ConnectionDB,
    stateConfigLog :: Log.Config,
    stateLogSettings :: Log.Settings,
    stateCache :: Cache.Cache
  }
  deriving (Show, Generic)

newtype ConnectionDB = ConnectionDB Connection

instance Show ConnectionDB where
  show _ = "connected"

-----------------------------Instances-----------------------------------------
instance MLog ServerStateIO where
  getSettings = ServerStateIO $ do
    gets stateLogSettings
  setSettings colorScheme logEnable = ServerStateIO $ do
    modify $ \state -> state {stateLogSettings = Log.Settings colorScheme logEnable}
  getConfig = ServerStateIO $ do
    gets stateConfigLog
  message logConfig logSettings logLevel str = ServerStateIO $ do
    Log.writeMessageIO logConfig logSettings logLevel str

instance MError ServerStateIO where
  throwServerError :: Error.Error -> ServerStateIO a
  throwServerError err = ServerStateIO $ do
    lift $ throwE err
  catchServerError :: ServerStateIO a -> (Error.Error -> ServerStateIO a) -> ServerStateIO a
  catchServerError ta f = ServerStateIO $ do
    StateT $ \state -> catchE (runStateT (getServerStateIO ta) state) $ \err ->
      runStateT (getServerStateIO $ f err) state

instance MIOError ServerStateIO

instance MDB ServerStateIO where
  mdbQuery query = do
    ConnectionDB connection <- ServerStateIO $ gets stateConnectionDB
    Error.liftEIO $ SQL.query_ connection query
  mdbExecute query = do
    ConnectionDB connection <- ServerStateIO $ gets stateConnectionDB
    Error.liftEIO $ SQL.execute_ connection query

instance MCache ServerStateIO where
  getCache = ServerStateIO $ gets stateCache
  setCache cache = ServerStateIO $ modify (\state -> state {stateCache = cache})

instance MTrans ServerStateIO