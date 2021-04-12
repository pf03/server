
module Logic.IO.File where

-- Our Modules
import           Common.Misc
import           Interface.Error            as Error
import           Interface.Log              as Log

-- Other Modules
import           Control.Exception          as Exception
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified System.Console.ANSI        as Color

import           Control.Monad.IO.Class
import           System.IO.Error

--переместить в какой-то другой модуль
-----------------работа с файлами-----------------------------------------------------------------------------------
-- readFile :: MIOError m => String -> m B.ByteString
-- readFile path = ExceptT $ toEE (BC.readFile path) `Exception.catch` handler where
--     handler :: IOException -> IO (EE B.ByteString )
--     handler e
--         | isDoesNotExistError e = return $ Left $ IOError $ template  "Файл \"{0}\" не найден!" [path]
--         | otherwise = return $ Left  $ IOError  $ template "Ошибка чтения файла \"{0}\"" [path]

read :: MIOError m => String -> m B.ByteString
read path = BC.readFile path `Error.catchEIO` handler where
    handler :: IOException -> E
    handler e
        | isDoesNotExistError e = IOError $ template  "Файл \"{0}\" не найден!" [path]
        | otherwise = IOError  $ template "Ошибка чтения файла \"{0}\"" [path]

writeResponse :: (MLog m, ToJSON a) => a -> m ()
writeResponse json = do
    Log.colorTextT Color.Yellow Log.Warning "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json --строгая версия
    --liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия

writeResponseJSON :: MLog m => LC.ByteString -> m ()
writeResponseJSON json = do
    Log.colorTextT Color.Yellow Log.Warning "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert json --строгая версия
    --liftIO $ B.appendFile "log.txt" $ convert ("\n" :: String)  --строгая версия
