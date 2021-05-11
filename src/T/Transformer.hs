module T.Transformer  where

-- Our Modules
import           Interface.Cache                  as Cache
import           Interface.Error                  as Error
import           Interface.Log                    as Log
import           T.State                          as S
import           Logic.IO.Config            as Config

-- Other Modules
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Database.PostgreSQL.Simple
import qualified System.Console.ANSI              as Color
import           System.Environment

-----------------------------External------------------------------------------
-- | Run and show result of transformer
showT :: Show a => T a -> IO ()
showT m = runE_ $ do
    config <- runConfig
    connection <- runConnection config
    value <- getValue config connection m
    showValue config value

-- | Run transformer without showing
runT :: T a -> IO ()
runT m = runE_ $ do
    config <- runConfig
    connection <- runConnection config
    _ <- getValue config connection m
    return ()

-- | Evaluate value of transformer with default value in error case
evalT :: T a -> a -> Config -> IO a
evalT m def config = runE def $ do
    connection <- runConnection config
    getValue config connection m

-- | Evaluate value of transformer with error handler
evalTwithHandler :: T a -> (E -> a) -> Config -> IO a
evalTwithHandler m handler config = runEwithHandler handler $ do
    connection <- runConnection config
    getValue config connection m

-----------------------------Internal------------------------------------------

runConfig :: (MIOError m) => m Config
runConfig = do
    let ls = Log.LogSettings Color.Cyan True
    config <- Error.catch readConfig $ \e -> do
        let dlc = Log.defaultConfig
        Log.critical dlc ls "Error config read while run the transfomer:"
        Log.critical dlc ls $ show e
        Error.throw e
    let lc = _log config
    Log.info lc ls "Config read successfully..."
    return config

runConnection :: (MIOError m) => Config -> m Connection
runConnection config = do
    let ls = Log.LogSettings Color.Cyan True
    let lc = _log config
    connection <- Error.catch (connectDB . getConnectInfo . _db $ config) $ \e -> do
        Log.critical lc ls "Error DB connection while start the transformer: "
        Log.critical lc ls $ show e
        Error.throw e
    Log.info lc ls "DB connected successfully..."
    return connection

getValue :: Config -> Connection -> T a -> ExceptT E IO a
getValue config connection m = do
    let s = getS config connection
    let ls = Log.LogSettings Color.Cyan True
    let lc = _log config
    a <-  Error.catch (runStateT m s) $ \e -> do
        Log.error lc ls "Application error: "
        Log.error lc ls $ show e
        Error.throw e
    return $ fst a

showValue :: (MonadIO m, Show a) => Config -> a -> m ()
showValue config value = do
    let ls = Log.LogSettings Color.Cyan True
    let lc = _log config
    Log.info lc ls "Result: "
    Log.info lc ls $ show value
    return ()

-----------------------------ExceptT E IO a------------------------------------
-- | Run ExceptT transformer without error handling with default value
runE :: a -> ExceptT E IO a -> IO a
runE a m = do
    eb <- runExceptT m
    case eb of
        Left _  -> return a
        Right b -> return b

-- | Run ExceptT transformer with error handling
runEwithHandler :: (E -> a) -> ExceptT E IO a -> IO a
runEwithHandler handler m = do
    eb <- runExceptT m
    case eb of
        Left e  -> return $ handler e
        Right b -> return b

-- | Value doesn't matter
runE_ :: ExceptT E IO () -> IO()
runE_ m = void (runExceptT m)

exceptToMaybe :: ExceptT E IO a -> IO (Maybe a)
exceptToMaybe  m = do
    ea <- runExceptT m
    case ea of
        Left e  -> return Nothing
        Right a -> return $ Just a

-----------------------------Config--------------------------------------------
getS :: Config -> Connection -> S
getS Config {_warp = cw, _db = _, _log = cl} connection = S {
    configWarp = cw,
    connectionDB = ConnectionDB connection,
    configLog = cl,
    logSettings = Log.defaultSettings,
    cache = Cache.defaultCache
}

-----------------------------DB------------------------------------------------
connectDB :: MIOError m => ConnectInfo -> m Connection
connectDB connectInfo = connect connectInfo `Error.catchEIO` handler where
    handler :: SqlError -> E
    handler _ = DBError "Error DB Connection!"

-----------------------------Log test------------------------------------------
testLog :: IO()
testLog = runT $ do
    Log.debugM $ "Debug data value " ++ show [1..10::Int]  :: T()
    Log.infoM $ "Info data value " ++ show [1..10::Int]
    Log.warnM  $ "warnM data value " ++ show [1..10::Int]
    Log.errorM $ "Error data value " ++ show [1..10::Int]
    Log.criticalM  $ "criticalM data value " ++ show [1..10::Int]
    Log.infoCM Color.Blue $ "Blue color scheme " ++ klichko
    Log.infoCM Color.Cyan $ "Cyan color scheme " ++ klichko
    Log.infoCM Color.Green $ "Green color scheme " ++ klichko
    Log.infoCM Color.Yellow $ "Yellow color scheme " ++ klichko
        where klichko = "Есть очень много по этому поводу точек зрения. Я четко придерживаюсь и четко понимаю, что те проявления, если вы уже так ребром ставите вопрос, что якобы мы"
