{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Insert 
    (user
    , author
    , category
    , tag
    , tagToContent
    , draft
    , publish
    , photos
    , comment
    , checkExist
    , rowEither
    ) where

import Data.Text (pack, Text(..))
import Types 
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common
import Query
import Control.Monad.Identity
import Select ( p, val, authUserIdParam, cond )
import Data.Map as M ((!), fromList)
import qualified Data.Map as M (insert)
import Transformer
import qualified Log
import Data.Maybe
import API
import qualified State as S
import Error

----------------------------------User-----------------------------------------
user :: T ()
user = do
    params <- S.getParams 
    checkNotExist "Пользователь" "login" [sql|SELECT 1 FROM users WHERE users.login = {0}|]
    insert User 
        [sql|INSERT into users (last_name, first_name, avatar, login, pass, creation_date, is_admin) values {0}|]
        [rowEither params 
            [ Left "last_name"
            , Left "first_name"
            , Left "avatar"
            , Left "login", 
            Right $ template [sql|md5 (CONCAT_WS(' ', {0}, {1})), current_date, FALSE|] 
                [cell(params ! "login"), cell(params ! "pass")]]] 

----------------------------------Author---------------------------------------
author :: T ()
author = do
    params <- S.getParams
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --проверка на то, что данный автор уже существует
    checkNotExist "Автор" "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    insert Author [sql|INSERT into authors (user_id, description)  values {0}|] 
        [row params ["user_id", "description"]]   

----------------------------------Category-------------------------------------
-- * нельзя вставить категорию с нeсуществующим родителем, но можно вставить 
-- категорию без родителя, "parent_id" - необязаельный параметр
category :: T ()
category = do
    params <- S.getParams
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    insert Category [sql|INSERT into categories (parent_id, category_name) values {0}|] 
        [row params ["parent_id", "category_name"]]

----------------------------------Tag------------------------------------------
tag :: T ()
tag = do
    params <- S.getParams
    checkNotExist "Тег" "name" [sql|SELECT 1 FROM tags WHERE tags.name = {0}|]
    insert Tag [sql|INSERT into tags (name)  values ({0})|] [p $ params ! "name"]

--сначала должна идти проверочная часть, потом часть с записью в бд
tagToContent :: Action -> T()
tagToContent Check = do
    params <- S.getParams
    let tagIds = valInt <$> (\(ParamAll list) -> list) (params ! "tag_id") -- :: [Val]
    checkExistAll "tag_id" tagIds $ [sql|SELECT id FROM tags|] `whereAll` [cond [sql|id|] $ ParamIn (Int <$> tagIds)]
tagToContent Execute = do
    params <- S.getParams
    unless (emptyParam $ params ! "tag_id") $ do
        execute__ [sql|INSERT into tags_to_contents (tag_id, content_id) values {0}|] [rows params ["tag_id", "content_id"]]

----------------------------------Draft----------------------------------------
draft :: T ()
draft = do
    params <- addAuthAuthorIdParam 
    when (params ! "author_id" == ParamEq (Int 1)) $ throwT $ DBError "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    Insert.tagToContent Check
    [Only cid] <- query_ $ template [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|] 
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]] :: T[Only Int]
    S.addChanged Insert Content 1
    S.addIdParam "content_id" cid
    Insert.tagToContent Execute
    Insert.photos
    insert Draft [sql|INSERT into drafts (content_id) values ({0})|] 
        [cell $ ParamEq (Int cid)]

----------------------------------Post-----------------------------------------
publish :: Int -> T ()
publish pid = do
    params <- S.addIdParam "draft_id" pid
    checkExist "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- query_ $ template [sql|SELECT content_id, post_id FROM drafts WHERE drafts.id = {0}|] 
        [p $ params ! "draft_id" ] :: T [(Int, Maybe Int)]
    case mpostId of 
        Nothing -> do 
            insert Post [sql|INSERT into posts (content_id) values ({0})|] [q contentId] --новость публикуется в первый раз
        Just postId -> do
            [Only oldContentId] <- query_ $ template [sql|SELECT content_id FROM posts WHERE posts.id = {0}|] [q postId] :: T [Only Int]
            update Post [sql|UPDATE posts SET content_id = {0} WHERE posts.id = {1}|] [q contentId, q postId]
            --контент привязан или к черновику или к посту, поэтому нигде больше не используется
            delete Content [sql|DELETE FROM contents WHERE contents.id = {0} |] [q oldContentId]
            execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q oldContentId]   
            delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q oldContentId]
    delete Draft [sql|DELETE FROM drafts WHERE drafts.id = {0}|] [p $ params ! "draft_id"]

----------------------------------Comment--------------------------------------
comment :: Int -> T ()
comment postId = do
    Insert.addAuthUserIdParam 
    params <- S.addIdParam "post_id" postId
    when (params ! "user_id" == ParamEq (Int 1)) $ throwT $ DBError "Невозможно создать комментарий от удаленного пользователя (пользователя по умолчанию) id = 1"
    checkExist "post_id" [sql|SELECT 1 FROM posts WHERE posts.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    insert Comment [sql|INSERT into comments (post_id, user_id, creation_date, text) values {0}|]
        [rowEither params [Left "post_id", Left "user_id", Right [sql|current_date|], Left "text"]]

----------------------------------Photo----------------------------------------
photos :: T ()
photos = do
    params <- S.getParams
    unless (emptyParam $ params ! "photos") $ do
        insert Photo [sql|INSERT into photos (photo, content_id) values {0}|] 
            [rows params ["photos", "content_id"]]

----------------------------------Common---------------------------------------
-- * в шаблон подставляется внутренний pid, если параметр обязательный, то ParamNo 
-- никогда не выскочит
checkExist :: BSName -> Query -> T() 
checkExist name templ = do
    param <- S.getParam name
    helper name param templ where
        helper name ParamNo templ = return ()
        helper name ParamNull templ = return ()
        helper name param@(ParamEq (Int pid)) templ = do 
            exist <- query_ $ template templ [q pid] :: T [Only Int]
            case exist of
                [] -> throwT $ DBError  (template "Указан несуществующий параметр {0}: {1}" [show name, show pid]) 
                _ -> return ()
                --x:xs -> ??
            --unless exist $ throwT $ DBError  (template "Уазан несуществующий parent_id: {0}" [show parentId]) 

--проверка на существование ВСЕХ сущностей из списка
checkExistAll :: BSName -> [Int] -> Query -> T() 
checkExistAll name all templ = do 
        --let tagIdVals = (\(ParamAll list) -> list) $ params ! "tag_id"
        -- let all = map valInt vals
        --exist <- fromOnly <<$>> (query_ $ templ) :: T [Only Int]
        exist <- fromOnly <<$>> query_ templ
        when (length exist /= length all) $ do
            let notExist = filter (`notElem` exist) all 
            throwT $ DBError $ template "Параметры {0} из списка {1} не существуют" [show name, show notExist]


checkNotExist :: String-> BSName -> Query -> T() 
checkNotExist description name templ = do 
    param <- S.getParam name
    helper name param templ where
        helper name ParamNo templ = return ()
        helper name param@(ParamEq v) templ = do 
            exist <- query_ $ template templ [val v] :: T [Only Int]
            case exist of
                [] -> return ()
                _ -> throwT $ DBError  (template "{2} с таким {0} = {1} уже существует" [show name, toString v, description]) 
        toString :: Val -> String 
        toString (Int n) = show n
        toString (Str s) = show s
        toString (Date d) = show d



row :: ParamsMap Param -> [BSName] -> Query
row params names = list $ map (\name -> cell (params ! name)) names where
    -- helper :: BSName -> Query
    -- helper name = case params ! name of
    --     ParamEq v -> val v
    --     ParamNo -> [sql|null|]

--количество строк определяется по первому параметру, который должен быть ParamAll! 
--должен быть хотя бы один параметр!
rows :: ParamsMap Param -> [BSName] -> Query
rows params names = res where
    (ParamAll first) = params ! head names
    len = length first
    listOfRows = map helper [0..len-1]
    helper :: Int -> Query
    helper n = list $ map (\name -> cellByNumber (params ! name) n) names
    res = Query.concat [sql|,|] listOfRows

emptyParam:: Param -> Bool 
emptyParam ParamNo = True 
emptyParam (ParamAll []) = True 
emptyParam (ParamIn []) = True
emptyParam _ = False


--обобщенный ряд
rowEither :: ParamsMap Param -> [Either BSName Query] -> Query
rowEither params nqs = list $ map helper nqs where
    helper :: Either BSName Query -> Query
    helper nq = case nq of 
        Left name -> cell (params ! name)
        Right q -> q


cell :: Param -> Query
cell (ParamEq v) = val v
cell ParamNo = [sql|null|]

cellByNumber :: Param -> Int -> Query
cellByNumber (ParamEq v) _ = val v
cellByNumber (ParamAll list) n = val (list !! n)
cellByNumber ParamNo _ = [sql|null|]


--админ может CОЗДАВАТЬ только свои публикации
addAuthAuthorIdParam :: T (ParamsMap Param)
addAuthAuthorIdParam = do
    Insert.addAuthUserIdParam
    paramUserId <- S.getParam "user_id"
    mauthorId <- fromOnly <<$>> listToMaybe  <$> query_ (template [sql|
    SELECT authors.id FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id
        WHERE users.id = {0}
    |] [p paramUserId]) :: T (Maybe Int)
    case mauthorId of
        Nothing -> throwT $ AuthError "Данная функция доступна только авторам"
        Just 1 -> throwT $ AuthError "Невозможна аутентификация удаленного автора"
        Just authorId -> S.addIdParam "author_id" authorId

--админ может СОЗДАВАТЬ только свои публикации (комментарии)
addAuthUserIdParam :: T (ParamsMap Param)
addAuthUserIdParam = do
    auth <- S.getAuth
    case auth of
        AuthAdmin userId -> S.addIdParam "user_id" userId
        AuthUser userId -> S.addIdParam "user_id" userId
        _ -> throwT authErrorDefault


