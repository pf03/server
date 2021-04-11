module Types.Transformer where

import qualified Log.Types as Log
import Cache

type T = StateT S (ExceptT E IO)
data S = S {
    configWarp :: ConfigWarp,
    connectionDB :: Connection,
    configLog :: Log.ConfigLog,
    logSettings :: Log.LogSettings,
    cache :: Cache
} deriving (Show, Generic)
