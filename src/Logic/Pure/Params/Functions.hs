module Logic.Pure.Params.Functions where

import Logic.Pure.Params.Internal ( possibleParamDescs, checkParams, parseParam, escapeQuotes ) 
import Common.Functions ( forMapWithKeyM )
import Interface.Class (MError)
import Interface.MCache.Types ( ParamsMap, API )
import Network.HTTP.Types.URI (Query)

parseParams :: MError m => API -> Query -> m ParamsMap
parseParams api queries = do
  paramDescs <- possibleParamDescs api
  checkParams api queries paramDescs
  let safeQueries = escapeQuotes queries
  forMapWithKeyM paramDescs $ parseParam safeQueries