{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Class where

import  qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Log
import Types
import qualified State as S
import Control.Exception
import Error
import Database.PostgreSQL.Simple
----------------------------------ToTransformer--------------------------------------------
--подъем до основного трасформера
class ToTransformer m where 
    toT :: m a -> T a

instance ToTransformer T where
    toT = id

instance ToTransformer (Except E) where
    toT = lift . except . runExcept 

instance ToTransformer (ExceptT E IO) where
    toT = lift

--эту идею обработки ошибок перенести в бота и в другие инстансы
--иначе зачем нам вообще Except
--ошибки sql почему то не ловит
instance ToTransformer IO where
    toT m = toT $ ExceptT $ toEE m `catch` iohandler `catch` sqlhandler `catch` otherhandler where
        --тут нужно как-то конкретизировать ошибку, хотя бы ее тип
        --handler :: Exception e => e -> IO (EE a)
        iohandler :: IOException -> IO (EE a)
        iohandler e = return . Left  . IOError . show $ e 
        sqlhandler :: SqlError -> IO (EE a)
        sqlhandler e = do
            return . Left . DBError . show $ e 
            --return . Left . DBError . BC.unpack . sqlErrorMsg $ e
        otherhandler :: SomeException -> IO (EE a)
        otherhandler e = return . Left . SomeError . show $ e
-- Note that we have to give a type signature to e , or the program will not 
-- typecheck as the type is ambiguous. While it is possible to catch exceptions of 
-- any type, see the section "Catching all exceptions" (in Control.Exception ) for an explanation of the problems with doing so.


--фантазия на тему обработки ошибок
--возможно инфиксная функция
--здесь указывается только тип ошибки
toTwithE_ :: ToTransformer m => m a -> (String -> E) -> T a
toTwithE_ = undefined 

--здесь указывается сама ошибка
toTwithE :: ToTransformer m => m a -> E -> T a
toTwithE = undefined 


instance ToTransformer (Reader S) where
    toT reader = do
        s <- get
        let res = runReader reader s
        return res

instance ToTransformer (State S) where
    toT state = do
        s <- get
        let a = evalState state s
        return a

instance Log.MonadLog T where 
  getSettings = S.getLogSettings
  setSettings = S.setLogSettings
  getConfig = S.getLogConfig


