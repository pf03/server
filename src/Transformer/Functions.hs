module Transformer.Functions (module Transformer.Functions, runConfig, exceptToMaybe) where

import qualified Interface.MError.Exports as Error
import qualified Logic.IO.Config as Config
import Transformer.Internal
    ( exceptToMaybe,
      runE_,
      runEWithHandler,
      runE,
      runConfig,
      runConnection,
      getValue,
      showValue )
import Transformer.Types (Transformer)

-- | Run transformer without showing
runT :: Transformer a -> IO ()
runT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  _ <- getValue config connection m
  return ()

-- | Evaluate value of transformer with error handler
evalTWithHandler :: Transformer a -> (Error.Error -> a) -> Config.Config -> IO a
evalTWithHandler m handler config = runEWithHandler handler $ do
  connection <- runConnection config
  getValue config connection m
