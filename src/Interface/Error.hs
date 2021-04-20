{-# LANGUAGE FlexibleInstances #-}
module Interface.Error where

import qualified Control.Exception          as E
import           Control.Monad.Except
import           Control.Monad.Trans.Except (catchE, throwE)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types


-----------------------------Types---------------------------------------------
data E = ParseError String
    | RequestError String
    | ConfigError String
    | DBError String
    | IOError String
    | AuthError String
    -- Ошибка, которая никогда не должна выскочить при правильном составлении программы 
    -- (например, некорректное сопоставление с образцом)
    | DevError String 
    | SomeError String

type EE = Either E

instance Show E where
    show (ParseError s)   = "Ошибка парсинга JSON: "++s
    show (RequestError s) = "Ошибка веб-запроса: "++s
    show (ConfigError s)  = "Ошибка конфигурации: "++s
    show (DBError s)      = "Ошибка базы данных: "++s
    show (IOError s)      = "Ошибка ввода-вывода: "++s
    show (AuthError s)    = "Ошибка авторизации: "++s
    show (DevError s)     = "Ошибка разработчика: "++s
    show (SomeError s)    = "Неведомая ошибка: "++s
instance E.Exception E

getStatus :: E -> Status
getStatus (ParseError _)   = internalServerError500  --есть ли такие ошибки??
getStatus (RequestError _) = badRequest400
getStatus (ConfigError _)  = internalServerError500
getStatus (DBError _)      = badRequest400
getStatus (IOError _)      = internalServerError500
getStatus (AuthError _)    = unauthorized401
getStatus (DevError _)     = internalServerError500
getStatus (SomeError _)    = internalServerError500

errorDefault :: E
errorDefault = SomeError "Ошибка по умолчанию"

authErrorDefault :: E
authErrorDefault = AuthError "Данная функция требует авторизации"

dbErrorDefault :: E
dbErrorDefault = DBError "Произошла ошибка базы данных. Обратитесь к администратору"

authErrorWrong :: E
authErrorWrong = AuthError "Неверная авторизация"

-----------------------------MError--------------------------------------------
class MonadFail m => MError m where
    throw :: E -> m a
    catch :: m a -> (E -> m a) -> m a

liftE ::  MError m => Either E a -> m a
liftE ea = case ea of
    Left e  -> throw e
    Right a -> return a

catchEither :: MError m => Either b a -> (b -> E) -> m a
catchEither eba handler = case eba of
        Left b  -> throw $ handler b
        Right a -> return a

-----------------------------MIOError------------------------------------------
class (MError m, MonadIO m) => MIOError m

catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> E) -> m a
catchEIO m h = do
    ea <- liftIO $  (Right <$> m) `E.catch` handler
    liftE ea
    where
    --handler :: Exception e => (e -> IO (EE a))
    handler e = return . Left  . h $ e

-- * То же, но ошибки не задаются пользователем, а обрабатываются автоматически
liftEIO :: MIOError m => IO a -> m a
liftEIO m = do
    ea <- liftIO $  (Right <$> m) `E.catch` iohandler `E.catch` sqlhandler `E.catch` otherhandler
    liftE ea
    where
    iohandler :: E.IOException -> IO (EE a)
    iohandler e = return . Left  . IOError . show $ e
    sqlhandler :: SqlError -> IO (EE a)
    sqlhandler e = return . Left . DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e
    otherhandler :: E.SomeException -> IO (EE a)
    otherhandler e = return . Left . SomeError . show $ e

-----------------------------Either E------------------------------------------
instance MonadFail (Either E) where
    fail s = Left $ DevError s

instance MError (Either E) where
    throw = Left
    catch ma f = case ma of
            Left e  -> f e
            Right a -> Right a

-----------------------------ExceptT E IO--------------------------------------
instance MError (ExceptT E IO) where
    throw = throwE
    catch = catchE

instance MIOError (ExceptT E IO)

-----------------------------DECODE--------------------------------------------
eDecode :: (MError m, FromJSON a) => LC.ByteString -> m a
eDecode bs = catchEither (eitherDecode bs) ParseError
