{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Logic.DB.Insert
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

-- Our Modules
import           Common.Misc
import           Interface.Cache                  as Cache
import           Interface.DB                     as DB
import           Interface.Error                  as Error
import           Logic.DB.Select                  (authUserIdParam, cond, p, val)

-- Other Modules
import           Control.Monad.Identity
import           Data.Map                         as M (fromList, (!))
import qualified Data.Map                         as M (insert)
import           Data.Maybe
import           Data.Text                        (Text (..), pack)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types as SQL


----------------------------------User-----------------------------------------
user :: MT m => m ()
user = do
    params <- Cache.getParams
    checkNotExist "Пользователь" "login" [sql|SELECT 1 FROM users WHERE users.login = {0}|]
    passQuery <- (return . template [sql|md5 (CONCAT_WS(' ', {0}, {1}))|]) <$$> [cell(params ! "login"), cell(params ! "pass")]
    DB.insert User
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
author :: MT m => m ()
author = do
    params <- Cache.getParams
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --проверка на то, что данный автор уже существует
    checkNotExist "Автор" "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    insert Author [sql|INSERT into authors (user_id, description)  values {0}|] <$$>
        [row params ["user_id", "description"]]

----------------------------------Category-------------------------------------
-- * Нельзя вставить категорию с нeсуществующим родителем, но можно вставить
-- категорию без родителя, "parent_id" - необязаельный параметр
category :: MT m => m ()
category = do
    params <- Cache.getParams
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    insert Category [sql|INSERT into categories (parent_id, category_name) values {0}|] <$$>
        [row params ["parent_id", "category_name"]]

----------------------------------Tag------------------------------------------
tag :: MT m => m ()
tag = do
    params <- Cache.getParams
    checkNotExist "Тег" "name" [sql|SELECT 1 FROM tags WHERE tags.name = {0}|]
    insert Tag [sql|INSERT into tags (name)  values ({0})|] [p $ params ! "name"]

--сначала должна идти проверочная часть, потом часть с записью в бд
tagToContent :: MT m => Action -> m ()
tagToContent Check = do
    params <- Cache.getParams
    let tagIds = valInt <$> (\(ParamAll list) -> list) (params ! "tag_id") -- :: [Val]
    c <- cond [sql|id|] $ ParamIn (Int <$> tagIds)
    checkExistAll "tag_id" tagIds $ [sql|SELECT id FROM tags|] `whereAll` [c]
tagToContent Execute = do
    params <- Cache.getParams
    unless (emptyParam $ params ! "tag_id") $ do
        execute__ [sql|INSERT into tags_to_contents (tag_id, content_id) values {0}|]
            [rows params ["tag_id", "content_id"]]

----------------------------------Draft----------------------------------------
draft :: MT m => m ()
draft = do
    params <- addAuthAuthorIdParam
    when (params ! "author_id" == ParamEq (Int 1)) $ Error.throw $ DBError
        "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    tagToContent Check
    [Only cid] <- (query_ . template
        [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|]) <$$>
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]]
    Cache.addChanged Insert Content 1
    Cache.addIdParam "content_id" cid
    tagToContent Execute
    photos
    insert Draft [sql|INSERT into drafts (content_id) values ({0})|] <$$>
        [cell $ ParamEq (Int cid)]

----------------------------------Post-----------------------------------------
publish :: MT m => Int -> m ()
publish pid = do
    params <- Cache.addIdParam "draft_id" pid
    checkExist "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- query_ $ template
        [sql|SELECT content_id, post_id FROM drafts WHERE drafts.id = {0}|]
        [p $ params ! "draft_id" ]
    case mpostId :: Maybe Int of
        Nothing -> do
            insert Post [sql|INSERT into posts (content_id) values ({0})|]
                [q (contentId :: Int)] --новость публикуется в первый раз
        Just postId -> do
            [Only oldContentId] <- DB.query_ $ template [sql|SELECT content_id FROM posts WHERE posts.id = {0}|]
                [q postId]
            update Post [sql|UPDATE posts SET content_id = {0} WHERE posts.id = {1}|] [q contentId, q postId]
            --контент привязан или к черновику или к посту, поэтому нигде больше не используется
            delete Content [sql|DELETE FROM contents WHERE contents.id = {0} |] [q (oldContentId :: Int)]
            execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q oldContentId]
            delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q oldContentId]
    delete Draft [sql|DELETE FROM drafts WHERE drafts.id = {0}|] [p $ params ! "draft_id"]

----------------------------------Comment--------------------------------------
comment :: MT m => Int -> m ()
comment postId = do
    addAuthUserIdParam
    params <- Cache.addIdParam "post_id" postId
    when (params ! "user_id" == ParamEq (Int 1)) $ Error.throw $ DBError
        "Невозможно создать комментарий от удаленного пользователя (пользователя по умолчанию) id = 1"
    checkExist "post_id" [sql|SELECT 1 FROM posts WHERE posts.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    insert Comment [sql|INSERT into comments (post_id, user_id, creation_date, text) values {0}|] <$$>
        [rowEither params [Left "post_id", Left "user_id", Right [sql|current_date|], Left "text"]]

----------------------------------Photo----------------------------------------
photos :: MT m => m ()
photos = do
    params <- Cache.getParams
    unless (emptyParam $ params ! "photos") $ do
        insert Photo [sql|INSERT into photos (photo, content_id) values {0}|]
            [rows params ["photos", "content_id"]]

----------------------------------Common---------------------------------------
-- * в шаблон подставляется внутренний pid, если параметр обязательный, то ParamNo
-- никогда не выскочит
checkExist :: MT m => BSName -> Query -> m ()
checkExist name templ = do
    param <- Cache.getParam name
    helper name param templ where
        helper name ParamNo templ = return ()
        helper name ParamNull templ = return ()
        helper name param@(ParamEq (Int pid)) templ = do
            exist <- DB.query_ $ template templ [q pid]
            case exist :: [Only Int] of
                [] -> Error.throw $ DBError $ template
                    "Указан несуществующий параметр {0}: {1}" [show name, show pid]
                _ -> return ()

-- | Проверка на существование ВСЕХ сущностей из списка
checkExistAll :: MT m => BSName -> [Int] -> Query -> m ()
checkExistAll name all templ = do
    exist <- fromOnly <<$>> DB.query_ templ
    when (length exist /= length all) $ do
        let notExist = filter (`notElem` exist) all
        Error.throw $ DBError $ template "Параметры {0} из списка {1} не существуют"
            [show name, show notExist]


checkNotExist :: MT m => String -> BSName -> Query -> m()
checkNotExist description name templ = do
    param <- Cache.getParam name
    helper name param templ where
        helper name ParamNo templ = return ()
        helper name param@(ParamEq v) templ = do
            exist <- query_ $ template templ [val v]
            case exist :: [Only Int] of
                [] -> return ()
                _ -> Error.throw $ DBError  (template "{2} с таким {0} = {1} уже существует"
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
    res = DB.concat [sql|,|] listOfRows

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
cell p           = Error.throw $ DevError $ template "Неверный шаблон параметра {0} в функции cell" [show p]

-- class Monad m => MonadError m where
--     throw :: m a
--     catch ::

cellByNumber :: Param -> Int -> Query
cellByNumber (ParamEq v) _     = val v
cellByNumber (ParamAll list) n = val (list !! n)
cellByNumber ParamNo _         = [sql|null|]


-- * Aдмин может CОЗДАВАТЬ только свои публикации
addAuthAuthorIdParam :: MT m => m (ParamsMap Param)
addAuthAuthorIdParam = do
    addAuthUserIdParam
    paramUserId <- Cache.getParam "user_id"
    mauthorId <- fromOnly <<$>> listToMaybe  <$> query_ (template [sql|
    SELECT authors.id FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id
        WHERE users.id = {0}
    |] [p paramUserId])
    case mauthorId :: (Maybe Int) of
        Nothing       -> Error.throw $ AuthError "Данная функция доступна только авторам"
        Just 1        -> Error.throw $ AuthError "Невозможна аутентификация удаленного автора"
        Just authorId -> Cache.addIdParam "author_id" authorId

-- | Админ может СОЗДАВАТЬ только свои публикации (комментарии)
addAuthUserIdParam :: (MError m, MCache m) => m (ParamsMap Param)
addAuthUserIdParam = do
    auth <- Cache.getAuth
    case auth of
        AuthAdmin userId -> Cache.addIdParam "user_id" userId
        AuthUser userId  -> Cache.addIdParam "user_id" userId
        _                -> Error.throw authErrorDefault


