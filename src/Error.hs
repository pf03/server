--importPriority = 70
module Error where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
import Types  --100
import Network.HTTP.Types
import Database.PostgreSQL.Simple
import qualified Data.Text.Encoding as T
import qualified Data.Text as T


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

instance Exception E

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

-------------------------------------------MError------------------------------------------

--проверить как MonadFail работает при неудачном сопоставлении с образцом
--и в случае необходимости переопределить функцию fail
class MonadFail m => MError m where
    throwM :: E -> m a
    catchM :: m a -> (E -> m a) -> m a
    liftE :: Either E a -> m a
    liftE ea = case ea of 
        Left e -> throwM e
        Right a -> return a



liftEIO :: (MError m, MonadIO m) => IO a -> m a
liftEIO m = do
    ea <- liftIO $  (Right <$> m) `catch` iohandler `catch` sqlhandler `catch` otherhandler 
    liftE ea

    where
    iohandler :: IOException -> IO (EE a)
    iohandler e = return . Left  . IOError . show $ e 
    sqlhandler :: SqlError -> IO (EE a)
    sqlhandler e = return . Left . DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e 
    otherhandler :: SomeException -> IO (EE a)
    otherhandler e = return . Left . SomeError . show $ e