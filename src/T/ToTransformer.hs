{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module T.ToTransformer where

-- Our Modules
import           Interface.Error            (E, EE)
import qualified Interface.Error            as Error
import           T.State                    as S

-- Other Modules
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as BC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Database.PostgreSQL.Simple

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
        iohandler e = return . Left  . Error.IOError . show $ e
        sqlhandler :: SqlError -> IO (EE a)
        sqlhandler e = do
            --return . Left . DBError . show $ e --wrong encoding
            --return . Left . DBError . BC.unpack . sqlErrorMsg $ e --not working
            return . Left . Error.DBError . T.unpack . T.decodeUtf8 . sqlErrorMsg $ e

        otherhandler :: SomeException -> IO (EE a)
        otherhandler e = return . Left . Error.SomeError . show $ e

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









