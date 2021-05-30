module Logic.DB.Select.Templates where

import Common.Functions (Template (template))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Query)
import Interface.Class ( MError ) 
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Templates ( inList, toQuery )
import qualified Interface.MError.Exports as Error

-----------------------------Templates-----------------------------------------
paramToQuery :: MError m => Cache.Param -> m Query
paramToQuery (Cache.ParamEq val) = return $ valToQuery val
paramToQuery param = Error.throw $ Error.patError "Select.paramToQuery" param

valToQuery :: Cache.Val -> Query
valToQuery (Cache.Int int) = toQuery int
valToQuery (Cache.Str str) = template [sql|'{0}'|] [toQuery str]
valToQuery (Cache.Date date) = template [sql|'{0}'|] [toQuery date]

paramToCondition :: MError m => Query -> Cache.Param -> m Query
paramToCondition field param = case param of
  Cache.ParamEq val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamIn vals -> return $ field `inList` map valToQuery vals
  Cache.ParamLt val -> return $ template [sql|{0} < {1}|] [field, valToQuery val]
  Cache.ParamGt val -> return $ template [sql|{0} > {1}|] [field, valToQuery val]
  Cache.ParamBt (val1, val2) -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, valToQuery val1, valToQuery val2]
  Cache.ParamLike (Cache.Str str) -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, toQuery str]
  Cache.ParamLike val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamNo -> return [sql|TRUE|]
  Cache.ParamNull -> return $ template [sql|{0} = null|] [field]
  _ -> Error.throw $ Error.patError "Select.paramToCondition" param