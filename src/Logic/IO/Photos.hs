module Logic.IO.Photos where

import Common.Types (FileName)
import Interface.Class (MCache, MIOError)
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types (Param (ParamEq), Val (Int, Str))
import qualified Interface.MError.Exports as Error
import qualified Logic.IO.File as File
import qualified Logic.IO.Upload as Upload
import Network.Wai as Wai (Request)
import System.Directory (listDirectory)

photosPath :: FilePath
photosPath = "photos"

-- | Uploading a photo to the server
upload :: (MIOError m, MCache m) => Request -> m ()
upload request = do
  ParamEq (Str name) <- Cache.getParam "name"
  freeName <- File.getFreeName photosPath name
  let path = photosPath <> "/" <> freeName
  Upload.saveBinary request path

-- | View photo (only error handling)
load :: MIOError m => FileName -> m ()
load name = Error.catch (File.checkExist photosPath name) $ \_ -> do
  Error.throwRequest "Photo {0} is not exist" [name]

-- | Returns photo filenames
select :: (MIOError m, MCache m) => m [FilePath]
select = do
  ParamEq (Int page) <- Cache.getParam "page"
  items <- Error.liftEIO $ listDirectory photosPath
  let quantity = 20
  return $
    take quantity . drop ((page -1) * quantity) $
      map (\item -> photosPath <> "/" <> item) items