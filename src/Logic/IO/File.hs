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
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified System.Console.ANSI        as Color
import           Control.Monad.IO.Class
import           System.IO.Error


read :: MIOError m => String -> m B.ByteString
read path = B.readFile path `Error.catchEIO` handler where
    handler :: IOException -> E
    handler e
        | isDoesNotExistError e = IOError $ template  "Файл \"{0}\" не найден!" [path]
        | otherwise = IOError  $ template "Ошибка чтения файла \"{0}\"" [path]

writeResponse :: (MonadIO m, MLog m, ToJSON a) => a -> m ()
writeResponse json = do
    Log.warnM "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json

writeResponseJSON :: (MonadIO m, MLog m) => LC.ByteString -> m ()
writeResponseJSON json = do
    Log.warnM "Запись ответа в файл в целях отладки..."
    liftIO $ B.writeFile "response.json" $ convert json
