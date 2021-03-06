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
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe

-- tag :: ParamsMap Param ->  Identity Query
-- tag params = return res where
--     ParamEq v = params ! "name"
--     res = template [sql|INSERT into tags (name) values ({0})|] [val v]

tag :: ParamsMap Param -> T ()
tag params = execute__ query where
    ParamEq v = params ! "name"
    query = template [sql|INSERT into tags (name)  values ({0})|] [val v]


--"user_id" - обязаельный параметр
author :: ParamsMap Param -> T()
author params = do 
    --checkUser $ params ! "user_id"
    checkExist params "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    execute__ $ template [sql|INSERT into authors (user_id, description)  values {0}|] [row params ["user_id", "description"]] 
    -- where
    --     checkUser :: Param -> T () 
    --     checkUser param@(ParamEq (Int userId)) = do 
    --         existUser <- isJust <$> Select.user param
    --         unless existUser $ throwT $ RequestError  (template "Уазан несуществующий user_id: {0}" [show userId])   

--нельзя вставить категорию с нeсуществующим родителем, но можно вставить категорию без родителя
--в первом случае возможно зациклить категории
--здесь идет не только запрос, но и некоторая предварительная обработка
--"parent_id" - не обязаельный параметр
category :: ParamsMap Param -> T()
category params = do
    --checkParent $ params ! "parent_id"
    checkExist params "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    execute__ $ template [sql|INSERT into categories (parent_id, category_name) values {0}|] [row params ["parent_id", "category_name"]] 
    where
        checkParent :: Param -> T () 
        checkParent ParamNo = return ()
        checkParent param@(ParamEq (Int parentId)) = do 
            existParent <- isJust <$> Select.category param
            unless existParent $ throwT $ RequestError  (template "Уазан несуществующий parent_id: {0}" [show parentId]) 

-- --наверно это чтото типа bracket
-- check :: Bool -> E -> T()
-- check cond error = do
--     --cond <- tcond
--     if cond then return () else throwT error


user :: ParamsMap Param -> T()
user = do
    undefined
    --checkUser $ 

--publish??
-- post :: ParamsMap Param -> T()
-- post params = do
--     checkContent $ params ! "content_id"
--     execute__ $ template [sql|INSERT into posts (content_id)  values ({0})|] [row params ["content_id"]]
--     where 
--         --сделать универсальну проверку - для случаев обязательного и необязательного параметра
--         checkContent :: Param -> T () 
--         checkContent param@(ParamEq (Int pid)) = do 
--             exist <- isJust <$> Select.content param
--             unless exist $ throwT $ RequestError  (template "Уазан несуществующий content_id: {0}" [show pid]) 

--сделать универсальну проверку - для случаев обязательного и необязательного параметра
--query Select 1 ...
--в шаблон подставляется внутренний pid
checkExist :: ParamsMap Param -> BSName -> Query -> T() 
checkExist params name templ = helper name (params ! name) templ where
    helper name ParamNo templ = return ()
    helper name param@(ParamEq (Int pid)) templ = do 
                exist <- query_ $ template templ [q pid] :: T [Only Int]
                case exist of
                    [] -> throwT $ RequestError  (template "Уазан несуществующий параметр {0}: {1}" [show name, show pid]) 
                    _ -> return ()
                    --x:xs -> ??
                --unless exist $ throwT $ RequestError  (template "Уазан несуществующий parent_id: {0}" [show parentId]) 
    


row :: ParamsMap Param -> [BSName] -> Query
row params names = list $ map helper names where
    helper :: BSName -> Query
    helper name = case params ! name of
        ParamEq v -> val v
        ParamNo -> [sql|null|]