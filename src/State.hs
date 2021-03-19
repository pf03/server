{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
module State where
import Types 
import qualified Data.Map.Internal as M
import qualified Log 

import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple
import API
import Data.Int
import Data.Char

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


--тут проще сделать через моноид!!
--сделать инстанс моноида для типа!!
--может он уже есть, проверить как работает
-- setChanged :: MonadState S m => QueryType -> APIType -> Int64 -> m ()
-- setChanged _ _ n |n <= 0 = return ();
-- setChanged Select _ _ = return ();
-- setChanged SelectById _ _ = return ();
-- setChanged _ (Id _) _ = return ();
-- setChanged Insert apiType n = do
--     mmodified <- gets $ \st -> M.lookup "edited" (changed st)
--     newModified <- case mmodified of 
--         Nothing -> return $ M.fromList [(entity apiType, n)]
--         Just modified -> case M.lookup (entity apiType) modified of
--             Nothing -> return $ M.insert (entity apiType) n modified
--             Just _ -> return $ M.adjust (+n) (entity apiType) modified
--     modify $ \st -> st {changed = M.insert "edited" newModified (changed st)}

--более элегантное решение через моноид
addChanged :: MonadState S m => QueryType -> APIType -> Int64 -> m ()
addChanged _ _ n |n <= 0 = return ();
addChanged _ (Id _) _ = return ();
addChanged Select _ _ = return ();
addChanged SelectById _ _ = return ();
addChanged qt at n = do
    let newChange = Changed $ M.fromList [(queryType qt, M.fromList [(apiTypes at, n)])]
    modify $ \st -> st {changed = changed st <> newChange} where

    apiTypes :: APIType -> String 
    apiTypes = plural . lower . show where
        plural :: String -> String
        plural str = case str of
            "category" -> "categories"
            _ -> str <> "s"
        lower (x:xs) = toLower x : xs

    queryType :: QueryType -> String 
    queryType Insert = "created"
    queryType Update = "edited"
    queryType Delete = "deleted"
    queryType Upload = "uploaded"


getChanged :: MonadState S m => m Changed 
getChanged = gets changed

resetChanged :: MonadState S m => m ()
resetChanged = modify $ \st -> st {changed = mempty}

instance Semigroup Changed where
    (<>) = mappend

instance Monoid Changed where 
    mempty = Changed mempty 
    mappend (Changed map1) (Changed map2) = Changed $ M.unionWith (M.unionWith (+)) map1 map2


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


