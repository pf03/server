--importPriority = 70

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Interface.Error where

import qualified Control.Exception as E
--import Control.Exception as Exception hiding (throw, catch)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Network.HTTP.Types
import Database.PostgreSQL.Simple
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC


-----------------------------Types---------------------------------------------
data E = ParseError String | RequestError String | ConfigError String | DBError String | IOError String | AuthError String | DevError String | SomeError String
-- это надо убрать, раз мы пользуемся мондами Parser - Except - ExceptT
type ES = Either String
type EE = Either E

instance Show E where
    show (ParseError s) = "Ошибка парсинга JSON: "++s
    show (RequestError s) = "Ошибка веб-запроса: "++s
    show (ConfigError s) = "Ошибка конфигурации: "++s
    show (DBError s) = "Ошибка базы данных: "++s
    show (IOError s) = "Ошибка ввода-вывода: "++s
    show (AuthError s) = "Ошибка авторизации: "++s
    show (DevError s) = "Ошибка разработчика: "++s
    show (SomeError s) = "Неведомая ошибка: "++s

getStatus :: E -> Status
getStatus (ParseError s) = internalServerError500  --есть ли такие ошибки??
getStatus (RequestError s) = badRequest400
getStatus (ConfigError s) = internalServerError500
getStatus (DBError s) = badRequest400
getStatus (IOError s) = internalServerError500
getStatus (AuthError s) = unauthorized401
getStatus (DevError s) = internalServerError500
getStatus (SomeError s) = internalServerError500
    

authErrorDefault :: E 
authErrorDefault = AuthError "Данная функция требует авторизации"

authErrorWrong :: E
authErrorWrong = AuthError "Неверная авторизация"

instance E.Exception E

--конкретизируем тип ошибки конструктором c
typeError :: (String -> E) -> ES a  -> EE a
typeError c (Left s) = Left $ c s
typeError _ (Right a) = Right a

toEE :: Monad m => m a -> m (EE a)
toEE x = return <$> x

toE :: Monad m => Except E a -> ExceptT E m a 
toE = except . runExcept

findPosition :: [Char] -> Integer
findPosition str = error "todo"

-------------------------------------------MError------------------------------

--проверить как MonadFail работает при неудачном сопоставлении с образцом
--и в случае необходимости переопределить функцию fail
class MonadFail m => MError m where
    throw :: E -> m a
    catch :: m a -> (E -> m a) -> m a

liftE ::  MError m => Either E a -> m a
liftE ea = case ea of 
    Left e -> throw e
    Right a -> return a

catchEither :: MError m => Either b a -> (b -> E) -> m a
catchEither eba handler = case eba of 
        Left b -> throw $ handler b
        Right a -> return a

-- catchEither :: Either b a -> (b -> Either c a) -> Either c a
-- catchEither eba handler = case eba of
--         Left b -> handler b
--         Right a -> Right a

--catch :: forall e a. Exception e => IO a -> (e -> IO a) -> IO a


-------------------------------------------MIOError------------------------------
class (MError m, MonadIO m) => MIOError m

--не проверено, но должно работать
catchEIO :: (MIOError m, E.Exception e) => IO a -> (e -> E) -> m a
catchEIO m h = do
    ea <- liftIO $  (Right <$> m) `E.catch` handler 
    liftE ea
    where
    --handler :: Exception e => (e -> IO (EE a))
    handler e = return . Left  . h $ e

--несколько последовательных обработчиков не подходят по сигнатуре
-- liftEIO :: MIOError m => IO a -> m a
-- liftEIO m = do
--     m `catchEIO` iohandler `catchEIO` sqlhandler --`catchEIO` otherhandler 
--     where
--     iohandler :: IOException -> E
--     iohandler e = IOError . show $ e 
--     sqlhandler :: SqlError -> E
--     sqlhandler e = DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e 
--     otherhandler :: SomeException -> E
--     otherhandler e = SomeError . show $ e


--то же, но ошибки не задаются пользователем, а обрабатываются автоматически
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

-------------------------------------------ExceptT E IO------------------------------
instance MError (ExceptT E IO) where
    throw = throwE 
    catch = catchE

instance MIOError (ExceptT E IO)

-----------------------------DECODE--------------------------------------------
eDecode :: (MError m, FromJSON a) => LC.ByteString -> m a
eDecode bs = catchEither (eitherDecode bs) ParseError