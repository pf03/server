module Logic.IO.Photos where

-- Our Modules
import           Common.Misc
import           Interface.Cache  as Cache
import           Interface.Error  as Error
import           Logic.IO.File    as File
import           Logic.IO.Upload  as Upload

-- Other Modules
import           Network.Wai      as Wai (Request)
import           System.Directory (listDirectory)


photosPath :: FilePath
photosPath = "photos"

-- | Uploading a photo to the server
upload :: (MIOError m, MCache m) => Request -> m ()
upload req = do
    ParamEq (Str name) <- Cache.getParam "name"
    freeName <- File.getFreeName photosPath name
    let path = photosPath <> "/" <> freeName
    Upload.saveBinary req path

-- | View photo (only error handling)
load :: MIOError m => FileName -> m ()
load name = Error.catch (File.checkExist photosPath name)  $ \_ -> do
        Error.throw $ RequestError $ template "Photo {0} is not exist" [name]

-- | Returns photo filenames
select :: (MIOError m, MCache m) => m [FilePath]
select = do
    ParamEq (Int page) <- Cache.getParam "page"
    items <- liftEIO $ listDirectory photosPath
    let quantity = 20
    return $ take quantity . drop ((page-1) * quantity) $
        map (\item -> photosPath <> "/" <> item) items

