module Interface.MError.Class where

import Control.Monad.Except (ExceptT, MonadIO (..))
import Control.Monad.Trans.Except (catchE, throwE)
import Interface.MError.Types (Error)

class MonadFail m => MError m where
  throwServerError :: Error -> m a
  catchServerError :: m a -> (Error -> m a) -> m a

class (MError m, MonadIO m) => MIOError m

instance MError (Either Error) where
  throwServerError = Left
  catchServerError ma f = case ma of
    Left err -> f err
    Right a -> Right a

instance MError (ExceptT Error IO) where
  throwServerError = throwE
  catchServerError = catchE

instance MIOError (ExceptT Error IO)