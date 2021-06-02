{-# LANGUAGE FlexibleInstances #-}

module Logic.IO.Upload where

import Interface.Class ( MIOError, MCache )
import Common.Functions (Template (template), putStrLnT)
import Common.Types (Path)
import Control.Monad.Except (MonadIO (..))
import qualified Data.ByteString as B
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types ( APIType(Photo), QueryType(Upload) )
import qualified Interface.MError.Exports as Error
import Network.Wai as Wai
  ( Request (requestBodyLength),
    RequestBodyLength (ChunkedBody, KnownLength),
    getRequestBodyChunk,
  )

saveBinary :: (MIOError m, MCache m) => Request -> Path -> m ()
saveBinary request path = do
  let fileSize = requestBodyLength request
  let maxFileSize = 5 * 1024 * 1024
  case fileSize of
    ChunkedBody ->
      Error.throwRequest "Unknown file size. Please select a file less than 5MB ({0} B)" [show maxFileSize]
    KnownLength n | n >= maxFileSize -> Error.throwRequest "The file size is {0} B. Please select a file less than 5 MB ({1} B)" 
        [show n, show maxFileSize]
    _ -> return ()
  Error.liftEIO $ B.writeFile path mempty
  stream 0 (getRequestBodyChunk request)
  where
    stream :: (MIOError m, MCache m) => Int -> IO B.ByteString -> m ()
    stream n str = do
      bs <- Error.liftEIO str
      if bs == mempty
        then do
          putStrLnT $ template "Successfully written {0} file parts" [show n]
          Cache.addChanged Upload Photo 1
        else do
          Error.liftEIO $ B.appendFile path bs
          stream (n + 1) str

-- | Reading the request body, divided into chunks
streamAll :: (MonadIO m, Eq a, Monoid a) => IO a -> (a -> IO ()) -> m ()
streamAll source0 = liftIO . helper (0 :: Int) source0
  where
    helper n source receiver = do
      a <- source
      if a == mempty
        then putStrLn $ template "Successfully read {0} parts of the request body" [show n]
        else do
          _ <- receiver a
          helper (n + 1) source receiver

-- | Reading the request body, divided into chunks - there should be no more than one chunk
streamOne :: (MIOError m, Eq a, Monoid a) => IO a -> m a
streamOne = helper (0 :: Int)
  where
    helper n source = do
      liftIO $ putStrLn "streamOne"
      a <- Error.liftEIO source
      if a == mempty
        then do
          liftIO $ putStrLn $ template "Successfully read {0} parts of the request body" [show n]
          return a
        else if n >= 1
            then Error.throwRequest "The request body is too long. The request body should consist of no more than one chunk" []
            else (a <>) <$> helper (n + 1) source

-- | Reading the request body, divided into chunks - should be empty
streamEmpty :: (MIOError m, Eq a, Monoid a) => IO a -> m ()
streamEmpty source = do
  a <- Error.liftEIO source
  if a == mempty
    then return ()
    else Error.throwRequest "Request body should be empty" []