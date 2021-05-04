module Logic.IO.Upload where
{-# LANGUAGE FlexibleInstances #-}

-- Our modules
import           Common.Misc
import           Interface.Cache      as Cache
import           Interface.Error      as Error

-- Other modules
import           Control.Monad.Except (MonadIO (..))
import qualified Data.ByteString      as B
import           Network.Wai          as Wai

saveBinary :: (MIOError m, MCache m) => Request -> Path -> m ()
saveBinary req path = do
    let fileSize =  requestBodyLength req
    let maxFileSize = 5*1024*1024
    case fileSize of
        ChunkedBody -> Error.throw $ RequestError $ 
            template "Unknown file size. Please select a file less than 5MB ({0} B)" [show maxFileSize]
        KnownLength  n| n >= maxFileSize -> Error.throw $ RequestError $ template "The file size is {0} B. Please select a file less than 5 MB ({1} B)" [show n, show maxFileSize]
        _ -> return()
    liftEIO $ B.writeFile path mempty
    stream 0 (getRequestBodyChunk req)
    where
    stream :: (MIOError m, MCache m) => Int -> IO B.ByteString -> m ()
    stream n str = do
        bs <- liftEIO str
        if bs == mempty
            then do
                putStrLnT $ template "Successfully written {0} file parts" [show n]
                Cache.addChanged Upload Photo 1
            else do
                liftEIO $ B.appendFile path bs
                stream (n+1) str

-- | Reading the request body, divided into chunks
streamAll :: (MonadIO m, Eq a, Monoid a) => IO a -> (a -> IO()) -> m ()
streamAll source0 = liftIO . helper (0 :: Int) source0 where
    helper n source receiver = do
        a <- source
        if a == mempty
            then putStrLn $ template "Successfully read {0} parts of the request body" [show n]
            else do
                _ <- receiver a
                helper (n+1) source receiver

-- | Reading the request body, divided into chunks - there should be no more than one chunk
streamOne :: (MIOError m, Eq a, Monoid a) => IO a -> m a
streamOne = helper (0 :: Int) where
    helper n source = do
        liftIO $ putStrLn "streamOne"
        a <- liftEIO source
        if a == mempty
            then do
                liftIO $ putStrLn $ template "Successfully read {0} parts of the request body" [show n]
                return a
            else if n>=1
                then Error.throw $ RequestError "The request body is too long. The request body should consist of no more than one chunk"
                else (a <>) <$> helper (n + 1) source

-- | Reading the request body, divided into chunks - should be empty
streamEmpty :: (MIOError m, Eq a, Monoid a) => IO a -> m ()
streamEmpty source = do
    a <- liftEIO source
    if a == mempty
        then return ()
        else Error.throw $ RequestError "Request body should be empty"
