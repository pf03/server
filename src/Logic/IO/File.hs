module Logic.IO.File where

import Common.Convert (Convert (convert))
import Common.Functions (breakOnLast)
import Common.Template (Template (template))
import Common.Types (BS, FileName, LBS)
import Control.Exception as Exception (IOException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as B
import Interface.Class (MIOError, MLog)
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import System.Directory (listDirectory)
import System.IO.Error (isDoesNotExistError)

read :: MIOError m => String -> m BS
read path = B.readFile path `Error.catchEIO` handler
  where
    handler :: IOException -> Error.Error
    handler err
      | isDoesNotExistError err = Error.IOError $ template "File \"{0}\" not found!" [path]
      | otherwise = Error.IOError $ template "File read error \"{0}\"" [path]

writeResponse :: (MonadIO m, MLog m, ToJSON a) => a -> m ()
writeResponse json = do
  Log.writeWarnM "Writing the response to the file for debug..."
  liftIO $ B.writeFile "response.json" $ convert . Aeson.encodePretty $ json

writeResponseJSON :: (MonadIO m, MLog m) => LBS -> m ()
writeResponseJSON json = do
  Log.writeWarnM "Writing the response to the file for debug..."
  liftIO $ B.writeFile "response.json" $ convert json

getFreeName :: (MIOError m) => FilePath -> FileName -> m FileName
getFreeName path fileName = do
  items <- Error.liftEIO $ listDirectory path
  let (name, extension) = breakOnLast '.' fileName
  let allNames =
        filter (`notElem` items) $
          fileName : map (\n -> name <> "_" <> show n <> "." <> extension) [1 :: Int, 2 ..]
  return $ head allNames

checkExist :: (MIOError m) => FilePath -> FileName -> m ()
checkExist path fileName = do
  items <- Error.liftEIO $ listDirectory path
  when (fileName `notElem` items) $ do
    Error.throwIO "File {0} does not exist in directory {1}" [fileName, path]
