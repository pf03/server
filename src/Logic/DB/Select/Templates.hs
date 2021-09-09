module Logic.DB.Select.Templates where

import Common.Template (Template (template))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Query)
import Interface.Class (MError)
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Templates (inList, toQuery)
import qualified Interface.MDB.Templates as DB
import qualified Interface.MError.Exports as Error

paramToQuery :: MError m => Cache.Param -> m Query
paramToQuery (Cache.ParamEq val) = return $ valToQuery val
paramToQuery param = Error.throw $ Error.patError "Select.paramToQuery" param

valToQuery :: Cache.ParamValue -> Query
valToQuery (Cache.IntParam int) = toQuery int
valToQuery (Cache.StringParam str) = template [sql|'{0}'|] [toQuery str]
valToQuery (Cache.DateParam date) = template [sql|'{0}'|] [toQuery date]

valListToQuery :: [Cache.ParamValue] -> Query
valListToQuery list = template [sql|'{{0}}'|] [DB.concatWith "," (map valToArrayItem list)]
  where
    valToArrayItem :: Cache.ParamValue -> Query
    valToArrayItem (Cache.IntParam int) = template [sql|{0}, |] [toQuery int]
    valToArrayItem (Cache.StringParam str) = template [sql|"{0}"|] [toQuery str]
    valToArrayItem (Cache.DateParam date) = template [sql|"{0}"|] [toQuery date]

paramToCondition :: MError m => Query -> Cache.Param -> m Query
paramToCondition field param = case param of
  Cache.ParamEq val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamIn vals -> return $ field `inList` map valToQuery vals
  Cache.ParamLt val -> return $ template [sql|{0} < {1}|] [field, valToQuery val]
  Cache.ParamGt val -> return $ template [sql|{0} > {1}|] [field, valToQuery val]
  Cache.ParamBt (val1, val2) -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, valToQuery val1, valToQuery val2]
  Cache.ParamLike (Cache.StringParam str) -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, toQuery str]
  Cache.ParamLike val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamNo -> return [sql|TRUE|]
  Cache.ParamNull -> return $ template [sql|{0} = null|] [field]
  _ -> Error.throw $ Error.patError "Select.paramToCondition" param