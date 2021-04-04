{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
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

import           API
import           Common
import           Control.Monad.Identity
import           Data.Map                         as M (fromList, (!))
import qualified Data.Map                         as M (insert)
import           Data.Maybe
import           Data.Text                        (Text (..), pack)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types as SQL
import           Error
import qualified Log
import           Query
import qualified Row
import           Select                           (authUserIdParam, cond, p, val)
import qualified State                            as S
import           Transformer
import           Types

----------------------------------User-----------------------------------------
user :: T ()
user = do
    params <- S.getParams
    checkNotExist "Пользователь" "login" [sql|SELECT 1 FROM users WHERE users.login = {0}|]
    passQuery <- (return . template [sql|md5 (CONCAT_WS(' ', {0}, {1}))|]) <$$> [cell(params ! "login"), cell(params ! "pass")]
    insert User
        [sql|INSERT into users (last_name, first_name, avatar, login, pass, creation_date, is_admin) values {0}|] <$$>
        [rowEither params
            [ Left "last_name"
            , Left "first_name"
            , Left "avatar"
            , Left "login"
            , Right passQuery
            , Right [sql|current_date|]
            , Right [sql|False|]
            ]]


----------------------------------Author---------------------------------------
author :: T ()
author = do
    params <- S.getParams
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --проверка на то, что данный автор уже существует
    checkNotExist "Автор" "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    insert Author [sql|INSERT into authors (user_id, description)  values {0}|] <$$>
        [row params ["user_id", "description"]]

----------------------------------Category-------------------------------------
-- * нельзя вставить категорию с нeсуществующим родителем, но можно вставить
-- категорию без родителя, "parent_id" - необязаельный параметр
category :: T ()
category = do
    params <- S.getParams
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    insert Category [sql|INSERT into categories (parent_id, category_name) values {0}|] <$$>
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
    checkExistAll "tag_id" tagIds $ [sql|SELECT id FROM tags|] `whereAll` 
        [cond [sql|id|] $ ParamIn (Int <$> tagIds)]
tagToContent Execute = do
    params <- S.getParams
    unless (emptyParam $ params ! "tag_id") $ do
        execute__ [sql|INSERT into tags_to_contents (tag_id, content_id) values {0}|] 
            [rows params ["tag_id", "content_id"]]

----------------------------------Draft----------------------------------------
draft :: T ()
draft = do
    params <- addAuthAuthorIdParam
    when (params ! "author_id" == ParamEq (Int 1)) $ throwT $ DBError 
        "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    Insert.tagToContent Check
    [Only cid] <- (query_ . template 
        [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|]) <$$>
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]]
    S.addChanged Insert Content 1
    S.addIdParam "content_id" cid
    Insert.tagToContent Execute
    Insert.photos
    insertM Draft [sql|INSERT into drafts (content_id) values ({0})|]
        [cell $ ParamEq (Int cid)]

----------------------------------Post-----------------------------------------
publish :: Int -> T ()
publish pid = do
    params <- S.addIdParam "draft_id" pid
    checkExist "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- query_ $ template 
        [sql|SELECT content_id, post_id FROM drafts WHERE drafts.id = {0}|]
        [p $ params ! "draft_id" ] :: T [(Int, Maybe Int)]
    case mpostId of
        Nothing -> do
            insert Post [sql|INSERT into posts (content_id) values ({0})|] 
                [q contentId] --новость публикуется в первый раз
        Just postId -> do
            [Only oldContentId] <- query_ $ template [sql|SELECT content_id FROM posts WHERE posts.id = {0}|] 
                [q postId] :: T [Only Int]
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
    when (params ! "user_id" == ParamEq (Int 1)) $ throwT $ DBError 
        "Невозможно создать комментарий от удаленного пользователя (пользователя по умолчанию) id = 1"
    checkExist "post_id" [sql|SELECT 1 FROM posts WHERE posts.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    insertM Comment [sql|INSERT into comments (post_id, user_id, creation_date, text) values {0}|]
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
                [] -> throwT $ DBError $ template 
                    "Указан несуществующий параметр {0}: {1}" [show name, show pid]
                _ -> return ()

-- | Проверка на существование ВСЕХ сущностей из списка
checkExistAll :: BSName -> [Int] -> Query -> T()
checkExistAll name all templ = do
    exist <- fromOnly <<$>> query_ templ
    when (length exist /= length all) $ do
        let notExist = filter (`notElem` exist) all
        throwT $ DBError $ template "Параметры {0} из списка {1} не существуют" 
            [show name, show notExist]


checkNotExist :: String-> BSName -> Query -> T()
checkNotExist description name templ = do
    param <- S.getParam name
    helper name param templ where
        helper name ParamNo templ = return ()
        helper name param@(ParamEq v) templ = do
            exist <- query_ $ template templ [val v] :: T [Only Int]
            case exist of
                [] -> return ()
                _ -> throwT $ DBError  (template "{2} с таким {0} = {1} уже существует" 
                    [show name, toString v, description])
        toString :: Val -> String
        toString (Int n)  = show n
        toString (Str s)  = show s
        toString (Date d) = show d



row :: MError m => ParamsMap Param -> [BSName] -> m Query
row params names = list <$> mapM (\name -> cell (params ! name)) names

-- * Количество строк определяется по первому параметру, который должен быть ParamAll
rows :: ParamsMap Param -> [BSName] -> Query
rows params names = res where
    (ParamAll first) = params ! head names
    len = length first
    listOfRows = map helper [0..len-1]
    helper :: Int -> Query
    helper n = list $ map (\name -> cellByNumber (params ! name) n) names
    res = Query.concat [sql|,|] listOfRows

emptyParam:: Param -> Bool
emptyParam ParamNo       = True
emptyParam (ParamAll []) = True
emptyParam (ParamIn [])  = True
emptyParam _             = False


-- | Обобщенный ряд
rowEither :: MError m => ParamsMap Param -> [Either BSName Query] -> m Query
rowEither params nqs = list <$> mapM helper nqs where
    helper :: MError m => Either BSName Query -> m Query
    helper nq = case nq of
        Left name -> cell (params ! name)
        Right q   -> return q


-- cell :: Param -> Query
-- cell (ParamEq v) = val v
-- cell ParamNo     = [sql|null|]

cell :: MError m => Param -> m Query
cell (ParamEq v) = return $ val v
cell ParamNo     = return [sql|null|]
cell p           = throwM $ DevError $ template "Неверный шаблон параметра {0} в функции cell" [show p]

-- class Monad m => MonadError m where
--     throw :: m a
--     catch :: 

cellByNumber :: Param -> Int -> Query
cellByNumber (ParamEq v) _     = val v
cellByNumber (ParamAll list) n = val (list !! n)
cellByNumber ParamNo _         = [sql|null|]


-- * Aдмин может CОЗДАВАТЬ только свои публикации
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
        Nothing       -> throwT $ AuthError "Данная функция доступна только авторам"
        Just 1        -> throwT $ AuthError "Невозможна аутентификация удаленного автора"
        Just authorId -> S.addIdParam "author_id" authorId

-- | Админ может СОЗДАВАТЬ только свои публикации (комментарии)
addAuthUserIdParam :: T (ParamsMap Param)
addAuthUserIdParam = do
    auth <- S.getAuth
    case auth of
        AuthAdmin userId -> S.addIdParam "user_id" userId
        AuthUser userId  -> S.addIdParam "user_id" userId
        _                -> throwT authErrorDefault


