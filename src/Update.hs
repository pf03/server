module Update where

import Database.PostgreSQL.Simple.FromRow --hiding (FromRow(..) ) 
import Database.PostgreSQL.Simple.Time
import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, Text(..))
import Types 
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common
import Query
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Identity
import Select ( p, val )
import Data.Map as M ((!), fromList)
import qualified Data.Map as M (insert)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe
import API
import Insert

tag :: Int -> ParamsMap Param -> T Changed
tag pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
    update Tag [sql|UPDATE tags SET name = {0} WHERE id = {1}|] [p $ params ! "name", q pid]

