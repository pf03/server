{-# LANGUAGE FlexibleInstances #-}

module Interface.MError.Class where

import Control.Monad.Except (ExceptT, MonadIO (..))
import Control.Monad.Trans.Except (catchE, throwE)
import Interface.MError.Types ( Error )

class MonadFail m => MError m where
  throw :: Error -> m a
  catch :: m a -> (Error -> m a) -> m a

class (MError m, MonadIO m) => MIOError m

instance MError (Either Error) where
  throw = Left
  catch ma f = case ma of
    Left err -> f err
    Right a -> Right a

instance MError (ExceptT Error IO) where
  throw = throwE
  catch = catchE

instance MIOError (ExceptT Error IO)