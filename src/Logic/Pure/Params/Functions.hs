module Logic.Pure.Params.Functions where

import Common.Functions (forMapWithKeyM)
import Interface.Class (MError)
import Interface.MCache.Types (API, ParamsMap)
import Logic.Pure.Params.Internal (checkParams, escapeQuotes, parseParam, possibleParamDescs)
import Network.HTTP.Types.URI (Query)

parseParams :: MError m => API -> Query -> m ParamsMap
parseParams api queries = do
  paramDescs <- possibleParamDescs api
  checkParams api queries paramDescs
  let safeQueries = escapeQuotes queries
  forMapWithKeyM paramDescs $ parseParam safeQueries