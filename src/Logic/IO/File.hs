module Logic.IO.File where

-- Our modules
import           Common.Misc
import           Interface.Error            as Error
import           Interface.Log              as Log

-- Other modules
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
        | isDoesNotExistError e = IOError $ template  "File \"{0}\" not found!" [path]
        | otherwise = IOError  $ template "File read error \"{0}\"" [path]

writeResponse :: (MonadIO m, MLog m, ToJSON a) => a -> m ()
writeResponse json = do
    Log.warnM "Writing the response to the file for debug..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json

writeResponseJSON :: (MonadIO m, MLog m) => LC.ByteString -> m ()
writeResponseJSON json = do
    Log.warnM "Writing the response to the file for debug..."
    liftIO $ B.writeFile "response.json" $ convert json
