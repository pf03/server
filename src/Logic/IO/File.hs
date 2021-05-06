module Logic.IO.File where

-- Our modules
import           Common.Misc
import           Interface.Error            as Error
import           Interface.Log              as Log

-- Other modules
import           Control.Exception          as Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString            as B
import           System.Directory
import           System.IO.Error

read :: MIOError m => String -> m BS
read path = B.readFile path `Error.catchEIO` handler where
    handler :: IOException -> E
    handler e
        | isDoesNotExistError e = IOError $ template  "File \"{0}\" not found!" [path]
        | otherwise = IOError  $ template "File read error \"{0}\"" [path]

writeResponse :: (MonadIO m, MLog m, ToJSON a) => a -> m ()
writeResponse json = do
    Log.warnM "Writing the response to the file for debug..."
    liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json

writeResponseJSON :: (MonadIO m, MLog m) => LBS -> m ()
writeResponseJSON json = do
    Log.warnM "Writing the response to the file for debug..."
    liftIO $ B.writeFile "response.json" $ convert json

getFreeName :: (MIOError m) => FilePath -> FileName -> m FileName
getFreeName path fileName = do
    items <- liftEIO $ listDirectory path
    let (name, extension) = splitOnLast '.' fileName
    let allNames = filter (`notElem` items) $ 
            fileName : map (\n -> name <> "_" <> show n <> "." <> extension) [1::Int,2..]
    return $ head allNames

checkExist :: (MIOError m) => FilePath -> FileName -> m ()
checkExist path fileName = do
    items <- liftEIO $ listDirectory path
    when (fileName `notElem` items) $ do
        Error.throw $ IOError $ template "File {0} is not exist in directory {1}" [fileName, path]
