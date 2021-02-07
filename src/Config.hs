--importPriority = 40
module Config 
where

--наши модули
import Error --70
import qualified Parse --50
import Types
import qualified Log
import Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Control.Exception
import System.IO.Error (isDoesNotExistError)
-- import qualified Data.Map.Internal as M
import Database.PostgreSQL.Simple

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy

-- этот модуль можно объединить с трансформер
-- потому что они оба нужны для запуска трансформера

readConfig :: ExceptT E IO Config
readConfig = do
    bs <- ExceptT $ toEE (L.readFile pathConfig) `catch` hR
    fileConfig <- toE $ Parse.eDecode bs
    --print fileConfig
    return fileConfig

--тут сделать обработку ошибок
connectDB :: ConnectInfo -> ExceptT E IO Connection
connectDB connectInfo = do
    conn <- ExceptT $ toEE (connect connectInfo) `catch` hC
    return conn

hC :: IOException -> IO (EE Connection )
hC e = return $ Left  $ DBError "Ошибка соединения с базой данных!"


readS :: ExceptT E IO S
readS = do
    config <- readConfig
    conn <- connectDB $ _db config 
    let s = toS config conn
    --print config
    return s

pathConfig :: FilePath
pathConfig = "config.json"

hR :: IOException -> IO (EE L.ByteString )
hR e
    | isDoesNotExistError e = return $ Left $ ConfigError "Файл конфигурации не найден!"
    | otherwise = return $ Left  $ ConfigError "Ошибка чтения файла конфигурации"





-------------------State <-> Config--------------------------------------
toS :: Config -> Connection -> S
toS Config {_warp = configWarp, _db = _, _log = configLog} connection = S {
    configWarp = configWarp,
    connectionDB = connection,
    configLog = configLog, 
    logSettings = Log.defaultSettings 
}

-- fromS :: Config -> S -> Config
-- fromS configFile config = undefined







