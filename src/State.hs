{-# LANGUAGE FlexibleContexts #-}
module State where
import Types 
import qualified Data.Map.Internal as M
import qualified Log 

import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple

--getters && setters lens like


getLogSettings :: MonadState S m => m Log.LogSettings
getLogSettings = gets logSettings

setLogSettings :: MonadState S m => Log.ColorScheme -> Log.Enable -> Log.FuncName -> m ()
setLogSettings cs e fn = modify $ \s -> s {logSettings = Log.LogSettings cs e fn}

getLogConfig :: MonadState S m => m Log.ConfigLog
getLogConfig = gets configLog

-- getConfigWarp :: MonadState S m => m ConfigWarp
-- getConfigWarp = gets configWarp --trivial

getWarpPort :: MonadState S m => m ConfigWarp
getWarpPort = gets configWarp --trivial

getConnection:: MonadState S m => m Connection
getConnection = gets connectionDB --trivial

-- getConfigDB:: MonadState S m => m ConnectInfo
-- getConfigDB = gets configDB --trivial

-- setConfigApp :: MonadState S m => ConfigApp -> m ()
-- setConfigApp ca  = modify $ \s -> s {configApp = ca}

-- getConfigText :: MonadState S m => m ConfigText
-- getConfigText = gets configText --trivial

-- getUpdateId :: MonadState S m => m UpdateId
-- getUpdateId = updateId <$> getConfigApp

-- setUpdateId :: MonadState S m => UpdateId -> m ()
-- setUpdateId uid = do
--     ca <- getConfigApp
--     setConfigApp ca {updateId = uid}
-- -- setUpdateIdT = toT . setUpdateId

-- getUpdateIdFromFile :: MonadState S m => MonadState S m => m Bool 
-- getUpdateIdFromFile = gets $ updateIdFromFile . configApp

-- getRepeatNumbers ::  MonadState S m => m (M.Map ChatId Int)
-- getRepeatNumbers = repeatNumber <$> getConfigApp

-- setRepeatNumbers ::  MonadState S m => M.Map ChatId Int -> m ()
-- setRepeatNumbers rns = do
--     ca <- getConfigApp
--     setConfigApp ca {repeatNumber = rns}

-- getmRepeatNumber :: MonadState S m => ChatId -> m (Maybe Int)
-- getmRepeatNumber cid = M.lookup cid <$> getRepeatNumbers

-- setRepeatNumber :: MonadState S m => ChatId -> Int -> m ()
-- setRepeatNumber cid rn = do
--     rns <- getRepeatNumbers
--     setRepeatNumbers $ M.insert cid rn rns


