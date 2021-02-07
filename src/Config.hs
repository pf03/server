--importPriority = 40
module Config 
where

--наши модули
-- import Error --70
-- import qualified Parse --50
-- import Types --100
import qualified Log
-- import Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Control.Exception
import System.IO.Error (isDoesNotExistError)
--import GHC.Generics
-- import qualified Data.Map.Internal as M

-- import Control.Monad.Trans.Except
-- import Control.Monad.Trans.Class
-- import Control.Monad.State.Lazy

readConfig :: ExceptT E IO Config
readConfig = do
    bs <- ExceptT $ toEE (L.readFile pathConfig) `catch` hR
    fileConfig <- toE $ Parse.eDecode bs
    --print fileConfig
    return fileConfig

readS :: ExceptT E IO S
readS = do
    config <- readConfig
    let s = toS config
    --print config
    return s

pathConfig :: FilePath
pathConfig = "config.json"

hR :: IOException -> IO (EE L.ByteString )
hR e
    | isDoesNotExistError e = throw $ ConfigError "Файл конфигурации не найден!"
    | otherwise = throw $ ConfigError "Ошибка чтения файла конфигурации"


-------------------State <-> Config--------------------------------------
toS :: Config -> S
toS configFile = undefined

fromS :: Config -> S -> Config
fromS configFile config = undefined







