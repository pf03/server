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
newtype Transformer a = Transformer {getTransformer :: StateT State (ExceptT Error.Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState State)

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
  throw :: Error.Error -> Transformer a
  throw err = Transformer $ do
    lift $ throwE err
  catch :: Transformer a -> (Error.Error -> Transformer a) -> Transformer a
  catch ta f = Transformer $ do
    StateT $ \state -> catchE (runStateT (getTransformer ta) state) $ \err ->
      runStateT (getTransformer $ f err) state

instance MIOError Transformer

instance MDB Transformer where
  dbQuery query = do
    ConnectionDB connection <- Transformer $ gets connectionDB
    Error.liftEIO $ SQL.query_ connection query
  dbExecute query = do
    ConnectionDB connection <- Transformer $ gets connectionDB
    Error.liftEIO $ SQL.execute_ connection query

instance MCache Transformer where
  getCache = Transformer $ gets cache
  setCache cache0 = Transformer $ modify (\state -> state {cache = cache0})

instance MTrans Transformer