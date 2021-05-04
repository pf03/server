module Logic.IO.Upload where
{-# LANGUAGE FlexibleInstances #-}


-- Our Modules
import           Common.Misc
import           Interface.Cache      as Cache
import           Interface.Error      as Error

-- Other Modules
import           Control.Monad.Except (MonadIO (..))
import qualified Data.ByteString      as B
import           Network.Wai          as Wai




-- | Сохранение фотографии
saveBinary :: (MIOError m, MCache m) => Request -> Path -> m ()
saveBinary req path = do
    let fileSize =  requestBodyLength req
    let maxFileSize = 5*1024*1024
    case fileSize of
        ChunkedBody -> Error.throw $ RequestError $ template "Неизвестный размер файла. Выберите файл меньше 5МБ ({0}Б)" [show maxFileSize]
        KnownLength  n| n >= maxFileSize -> Error.throw $ RequestError $ template "Размер файла составляет {0} Б . Выберите файл меньше 5 МБ ({1} Б)" [show n, show maxFileSize]
        _ -> return()
    liftEIO $ B.writeFile path mempty
    stream 0 (getRequestBodyChunk req)
    where
    stream :: (MIOError m, MCache m) => Int -> IO B.ByteString -> m ()
    stream n str = do
        bs <- liftEIO str
        if bs == mempty
            then do
                putStrLnT $ template "Успешно записано {0} чаcтей файла" [show n]
                Cache.addChanged Upload Photo 1
            else do
                liftEIO $ B.appendFile path bs
                stream (n+1) str

-- | Для работы с телом запроса, поделенным на чанки.
streamAll :: (MonadIO m, Eq a, Monoid a) => IO a -> (a -> IO()) -> m ()
streamAll source0 = liftIO . helper (0 :: Int) source0 where
    helper n source receiver = do
        a <- source
        if a == mempty
            then putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
            else do
                _ <- receiver a
                helper (n+1) source receiver

-- | Для работы с телом запроса, поделенным на чанки - должно быть не более одного чанка
streamOne :: (MIOError m, Eq a, Monoid a) => IO a -> m a
streamOne = helper (0 :: Int) where
    helper n source = do
        liftIO $ putStrLn "streamOne"
        a <- liftEIO source
        if a == mempty
            then do
                liftIO $ putStrLn $ template "Успешно прочитано {0} чаcтей тела запроса" [show n]
                return a
            else if n>=1
                then Error.throw $ RequestError "Слишком длинное тело запроса. Тело запроса должно состоять не более, чем из одного чанка"
                else (a <>) <$> helper (n + 1) source

-- | Для работы с телом запроса, поделенным на чанки - должно быть пустым
streamEmpty :: (MIOError m, Eq a, Monoid a) => IO a -> m ()
streamEmpty source = do
    a <- liftEIO source
    if a == mempty
        then return ()
        else Error.throw $ RequestError "Тело запроса должно быть пустым"
