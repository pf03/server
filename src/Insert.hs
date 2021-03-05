{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Insert where

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
import Select
import Data.Map as M ((!))
import Class

-- tag :: ParamsMap Param ->  Identity Query
-- tag params = return res where
--     ParamEq v = params ! "name"
--     res = template [sql|INSERT into tags (name) values ({0})|] [val v]

tag :: ParamsMap Param -> T ()
tag params = execute__ query where
    ParamEq v = params ! "name"
    query = template [sql|INSERT into tags (name)  values ({0})|] [val v]

author :: ParamsMap Param -> T()
author params = execute__ query where
    query = template [sql|INSERT into authors (user_id, description)  values {0}|] [vals params ["user_id", "description"]]

--нельзя вставить категорию с нeсуществующим родителем, но можно вставить категорию без родителя
--в первом случае возможно зациклить категории
category :: ParamsMap Param -> T()
category params = execute__ query where

    query = template [sql|INSERT into categories (parent_id, category_name)  values {0}|] [vals params ["parent_id", "category_name"]]

vals :: ParamsMap Param -> [BSName] -> Query
vals params names = list $ map helper names where
    helper :: BSName -> Query
    helper name = res where
        ParamEq v = params ! name
        res = val v