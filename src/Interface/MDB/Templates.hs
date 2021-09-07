module Interface.MDB.Templates where

import Common.Convert (Convert (..))
import Common.Functions ((<$$>))
import Common.Template (Template (template))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))

-- Various templates for composing complex queries from simple pieces

whereAll :: Query -> [Query] -> Query
whereAll query0 conditions = concat2 [sql|WHERE|] query0 $ concatWithAND conditions

whereAllM :: Monad m => Query -> [m Query] -> m Query
whereAllM query0 mConditions = do
  (return . concat2 [sql|WHERE|] query0 . concatWithAND) <$$> mConditions

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
(<+>) q1 q2 | q1 == mempty = q2
(<+>) q1 q2 | q2 == mempty = q1
(<+>) q1 q2 = q1 <> " " <> q2

(<<+>>) :: Monad m => m Query -> m Query -> m Query
(<<+>>) mQuery1 mQuery2 = (<+>) <$> mQuery1 <*> mQuery2

inList :: Convert a => Query -> [a] -> Query
inList _ [] = "FALSE"
inList field values = template [sql|{0} IN ({1})|] [field, concatWith "," $ map toQuery values]

inSubQuery :: Query -> Query -> Query
inSubQuery field subQuery = template [sql|{0} IN ({1})|] [field, subQuery]

inSubQueryM :: Monad m => Query -> m Query -> m Query
inSubQueryM field subQuery = return . template [sql|{0} IN ({1})|] <$$> [return field, subQuery]

exists :: Query -> Query
exists query0 = template [sql|EXISTS ({0})|] [query0]

brackets :: Query -> Query
brackets query0 = template [sql|({0})|] [query0]

list :: [Query] -> Query
list queries = template [sql|({0})|] [concatWith "," queries]

toQuery :: Convert a => a -> Query
toQuery = Query . convert