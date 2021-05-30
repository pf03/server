module Interface.MDB.Templates where 

import Common.Convert (Convert (..))
import Common.Functions (Template (template), (<$$>))
-- import Data.Int (Int64)
-- import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
-- import qualified Interface.MCache.Exports as Cache
-- import Interface.MDB.Class (MDB (..), MTrans)
-- import qualified Interface.MError.Exports as Error
-- import qualified Interface.MLog.Exports as Log


-- Various templates for composing complex queries from simple pieces

whereAll :: Query -> [Query] -> Query
whereAll query0 conditions = concat2 [sql|WHERE|] query0 $ concatWithAND conditions

whereAllM ::Monad m => Query -> [m Query] -> m Query
whereAllM query0 mconditions = do
  (return . concat2 [sql|WHERE|] query0 . concatWithAND) <$$> mconditions

concatWithOR :: [Query] -> Query
concatWithOR = concatWith [sql|OR|]

concatWithAND :: [Query] -> Query
concatWithAND = concatWith [sql|AND|]

concatWith :: Query -> [Query] -> Query
concatWith _ [] = mempty
concatWith _ [x] = x
concatWith splitter (x : xs) = concat2 splitter x (concatWith splitter xs)

concat2 :: Query -> Query -> Query -> Query
concat2 _ query1 query2 | query1 == mempty = query2
concat2 _ query1 query2 | query2 == mempty = query1
concat2 splitter query1 query2 = query1 <+> splitter <+> query2

(<+>) :: Query -> Query -> Query
(<+>) = concat2 " "

(<<+>>) :: Monad m => m Query -> m Query -> m Query
(<<+>>) mquery1 mquery2 = (<+>) <$> mquery1 <*> mquery2

inList :: Convert a => Query -> [a] -> Query
inList _ [] = "FALSE"
inList field values = template [sql|{0} IN ({1})|] [field, concatWith "," $ map toQuery values]

inSubquery :: Query -> Query -> Query
inSubquery field subquery = template [sql|{0} IN ({1})|] [field, subquery]

inSubqueryM :: Monad m => Query -> m Query -> m Query
inSubqueryM field subquery = return . template [sql|{0} IN ({1})|] <$$> [return field, subquery]

exists :: Query -> Query
exists query0 = template [sql|EXISTS ({0})|] [query0]

brackets :: Query -> Query
brackets query0 = template [sql|({0})|] [query0]

list :: [Query] -> Query
list queries = template [sql|({0})|] [concatWith "," queries]

toQuery :: Convert a => a -> Query
toQuery = Query . convert