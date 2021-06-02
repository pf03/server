module Transformer.Functions (module Transformer.Functions, runConfig, exceptToMaybe) where

import qualified Interface.MError.Exports as Error
import qualified Logic.IO.Config as Config
import Transformer.Internal
    ( exceptToMaybe,
      runE_,
      runEwithHandler,
      runE,
      runConfig,
      runConnection,
      getValue,
      showValue )
import Transformer.Types (Transformer)

-- | Run and show result of transformer
showT :: Show a => Transformer a -> IO ()
showT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  value <- getValue config connection m
  showValue config value

-- | Run transformer without showing
runT :: Transformer a -> IO ()
runT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  _ <- getValue config connection m
  return ()

-- | Evaluate value of transformer with default value in error case
evalT :: Transformer a -> a -> Config.Config -> IO a
evalT m defaultValue config = runE defaultValue $ do
  connection <- runConnection config
  getValue config connection m

-- | Evaluate value of transformer with error handler
evalTwithHandler :: Transformer a -> (Error.Error -> a) -> Config.Config -> IO a
evalTwithHandler m handler config = runEwithHandler handler $ do
  connection <- runConnection config
  getValue config connection m
