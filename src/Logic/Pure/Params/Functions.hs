module Logic.Pure.Params.Functions where

import Logic.Pure.Params.Internal ( possibleParamDescs, checkParams, parseParam )
import Common.Functions ( forMapWithKeyM )
import Interface.Class (MError)
import Interface.MCache.Types ( ParamsMap, API )
import Network.HTTP.Types.URI (Query)

parseParams :: MError m => API -> Query -> m ParamsMap
parseParams api queries = do
  paramDescs <- possibleParamDescs api
  checkParams api queries paramDescs
  forMapWithKeyM paramDescs $ parseParam queries