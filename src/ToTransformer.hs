{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ToTransformer where

import  qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Log
import Types
import qualified State as S
import Control.Exception
import qualified Error
import Database.PostgreSQL.Simple

import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Control.Monad.Identity
--этот модуль практически становится ненужным по мере перехода на классы типов
----------------------------------ToTransformer--------------------------------------------
--подъем до основного трасформера
class ToTransformer m where 
    toT :: m a -> T a

instance ToTransformer T where
    toT = id

instance ToTransformer (Except E) where
    toT = lift . except . runExcept 

instance ToTransformer (Either E) where
    toT = toT . helper where
        helper :: Either E a -> Except E a
        helper = except

instance ToTransformer (ExceptT E IO) where
    toT = lift

instance ToTransformer Identity where
    toT = return . runIdentity

-- instance MonadIO m => ToTransformer m where
--     toT m = undefined

--эту идею обработки ошибок перенести в бота, иначе зачем нам вообще Except
instance ToTransformer IO where
    toT m = toT $ ExceptT $ Error.toEE m `catch` iohandler `catch` sqlhandler `catch` otherhandler where
        iohandler :: IOException -> IO (EE a)
        iohandler e = return . Left  . IOError . show $ e 
        sqlhandler :: SqlError -> IO (EE a)
        sqlhandler e = do
            --return . Left . DBError . show $ e --wrong encoding
            --return . Left . DBError . BC.unpack . sqlErrorMsg $ e --not working
            return . Left . DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e
            
        otherhandler :: SomeException -> IO (EE a)
        otherhandler e = return . Left . SomeError . show $ e

--toT++log -- в логах есть аналог logM
-- logT :: (ToTransformer m, Show a)  => m a -> T a 
-- logT m = do
--     a <- toT m
--     Log.debugT a
--     return a

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









