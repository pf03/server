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
import Select

user :: Int -> ParamsMap Param -> T Changed
user pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --{0} может быть пустым
    update User [sql|UPDATE users SET {0} WHERE id = {1}|] 
        [updates params ["first_name", "last_name", "avatar", "pass"], q pid]
        --[updates params []]

author :: Int -> ParamsMap Param -> T Changed
author pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
    checkExist allParams "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    update Author [sql|UPDATE authors SET {0} WHERE id = {1}|] 
        [updates params ["user_id", "description"], q pid]

category :: Int -> ParamsMap Param -> T Changed
category pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    checkExist allParams "parent_id" [sql|SELECT 1 FROM categories WHERE categories.parent_id = {0}|]
    update Category [sql|UPDATE categories SET {0} WHERE id = {1}|] 
        [updates params ["parent_id", "category_name"], q pid]

draft :: Int -> ParamsMap Param -> T Changed
draft pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    checkExist allParams "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    [Only contentId] <- query_ $ template [sql|SELECT content_id FROM drafts WHERE drafts.id = {0}|] [q pid] :: T [Only Int]
    update Content [sql|UPDATE contents SET {0} WHERE id = {1}|] 
        [updates params ["name", "category_id", "text", "photo"], q contentId]

-- см. Insert.draft
-- post :: Int -> ParamsMap Param -> T Changed
-- post pid params = do
--     let allParams = M.insert "id" (ParamEq (Int pid)) params
--     undefined

tag :: Int -> ParamsMap Param -> T Changed
tag pid params = do
    let allParams = M.insert "id" (ParamEq (Int pid)) params
    checkExist allParams "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
    update Tag [sql|UPDATE tags SET name = {0} WHERE id = {1}|] [p $ params ! "name", q pid]


--упорядочить эти функции
updates :: ParamsMap Param -> [BSName] -> Query
updates params names = Query.concat "," $ mapMaybe helper names where
    helper :: BSName -> Maybe Query
    helper name = upd (q name) (params ! name)

    upd :: Query -> Param -> Maybe Query 
    upd "pass" (ParamEq v) = Just $ template [sql|pass = md5({0})|] [val v] --костыль для pass
    upd field (ParamEq v) = Just $ template [sql|{0} = {1}|] [field, val v]
    upd field ParamNo = Nothing 
    upd field ParamNull = Just $ template [sql|{0} = null|] [field]
    upd field param = error $ template "Нет шаблона для {0}" [show param]

