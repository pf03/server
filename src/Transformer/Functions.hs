module Transformer.Functions (module Transformer.Functions, runConfig, exceptToMaybe) where

import Database.PostgreSQL.Simple (close)
import qualified Interface.MError.Exports as Error
import qualified Logic.IO.Config as Config
import Transformer.Internal
  ( exceptToMaybe,
    getValue,
    runConfig,
    runConnection,
    runEWithHandler,
    runE_,
  )
import Transformer.Types (Transformer)

-- | Run transformer without showing
runT :: Transformer a -> IO ()
runT m = runE_ $ do
  config <- runConfig
  connection <- runConnection config
  _ <-
    getValue config connection m `Error.catchServerError` \err -> do
      Error.liftEIO $ close connection
      Error.throwServerError err
  Error.liftEIO $ close connection

-- | Evaluate value of transformer with error handler
evalTWithHandler :: Transformer a -> (Error.Error -> a) -> Config.Config -> IO a
evalTWithHandler m handler config = runEWithHandler handler $ do
  connection <- runConnection config
  value <-
    getValue config connection m `Error.catchServerError` \err -> do
      Error.liftEIO $ close connection
      Error.throwServerError err
  Error.liftEIO $ close connection
  return value
