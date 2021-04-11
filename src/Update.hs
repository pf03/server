module Update
    (user
    , author
    , category
    , tag
    , draft
    , checkAuthExistDraft
    , post
    , checkAuthExistPost
    , checkAuthExistComment
    ) where

import           API
import           Common                           (Template (template), (<$$>), mapMaybeM)
import           Control.Monad.Identity           (when)
import           Data.Map                         as M (fromList, (!))
import qualified Data.Map                         as M (insert)
import           Data.Maybe
import           Data.Text                        (Text (..), pack)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types as SQL (Only (Only), Query)
import           Error
import           Insert                           (checkExist, photos,
                                                   rowEither, tagToContent)
import qualified Log
import           DB
import qualified Row
import           Select                           (cond, p, val)
import qualified State                            as S
import           Transformer
import           Common
import           Cache
----------------------------------User-----------------------------------------
user :: MT m => Int -> m ()
user pid = do
    Cache.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    --логин нужен для проверки пароля
    [Only login] <- query_ $ template [sql|SELECT users.login FROM users WHERE users.id = {0}|] [q pid]
    params <- Cache.addStrParam "login" login
    DB.update User [sql|UPDATE users SET {0} WHERE id = {1}|] <$$>
        [updates params ["first_name", "last_name", "avatar", "pass"], return $ q pid]

----------------------------------Author---------------------------------------
author :: MT m => Int -> m ()
author pid = do
    params <- Cache.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    DB.update Author [sql|UPDATE authors SET {0} WHERE id = {1}|] <$$>
        [updates params ["user_id", "description"], return $ q pid]

----------------------------------Category-------------------------------------
category :: MT m => Int -> m ()
category pid = do
    params <- Cache.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.parent_id = {0}|]
    DB.update Category [sql|UPDATE categories SET {0} WHERE id = {1}|] <$$>
        [updates params ["parent_id", "category_name"], return $ q pid]

----------------------------------Tag------------------------------------------
tag :: MT m => Int -> m ()
tag pid = do
    params <- Cache.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
    DB.update Tag [sql|UPDATE tags SET name = {0} WHERE id = {1}|] [p $ params ! "name", q pid]

tagToContent :: MT m => Action -> m ()
tagToContent Check = withParam "tag_id" $ Insert.tagToContent Check
tagToContent Execute = withParam "tag_id" $ do
    ParamEq (Int cid) <- Cache.getParam "content_id"
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q cid]
    Insert.tagToContent Execute

----------------------------------Draft----------------------------------------
draft :: MT m => Int-> m ()
draft pid = do
    checkAuthExistDraft pid
    params <- Cache.getParams
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    Update.tagToContent Check
    ParamEq (Int cid) <- Cache.getParam "content_id"
    DB.update Content [sql|UPDATE contents SET {0} WHERE id = {1}|] <$$>
        [updates params ["name", "category_id", "text", "photo"], return $ q cid]
    Update.tagToContent Execute
    Update.photos

--проверка существования вместе с авторизацией, для запросов DB.update и delete
checkAuthExistDraft :: MT m => Int -> m (ParamsMap Param)
checkAuthExistDraft pid = do
    query <- [sql|
        SELECT users.id, authors.id, contents.id FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |] `whereAllM` [cond [sql|drafts.id|] $ ParamEq (Int pid)]
    (uid, aid, cid) <- checkAuthExist pid "draft_id" query
    Cache.addIdParam "id" pid
    Cache.addIdParam "user_id" uid
    Cache.addIdParam "author_id" aid
    Cache.addIdParam "content_id" cid

----------------------------------Post-----------------------------------------
post :: MT m => Int -> m ()
post pid = do
    params <- checkAuthExistPost pid
    when (params ! "author_id" == ParamEq (Int 1)) $ 
        Error.throw $ DBError "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    Insert.tagToContent Check
    [Only cid] <- query_ . template 
        [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|] <$$>
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]]
    Cache.addChanged Insert Content 1
    DB.insert Draft [sql|INSERT into drafts (content_id, post_id) values ({0}, {1})|]
        [q cid, q pid]
    Cache.addIdParam "content_id" cid --перезаписываем
    Insert.tagToContent Execute
    Insert.photos

-- * Аутентификация удаленным пользователем не пройдет. Удаленный автор привязан к удаленному
-- пользователю. Таким образом посты с удаленными авторами и пользователями сможет редактировать
-- только админ
checkAuthExistPost :: MT m => Int -> m (ParamsMap Param)
checkAuthExistPost pid = do
    query <- [sql|
        SELECT users.id, authors.id, contents.id FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |] `whereAllM` [cond [sql|posts.id|] $ ParamEq (Int pid)]
    (uid, aid, cid) <- checkAuthExist pid "post_id" query
    Cache.addIdParam "id" pid
    Cache.addIdParam "user_id" uid
    Cache.addIdParam "author_id" aid
    Cache.addIdParam "content_id" cid

----------------------------------Comment--------------------------------------
-- * В общем случае checkAuthExist возвращает три параметра, поэтому здесь небольшой костыль
checkAuthExistComment :: MT m => Int -> m Int --возвращаем userId
checkAuthExistComment pid = do
    let query = template [sql| SELECT user_id, 0, 0 FROM comments WHERE id = {0}|] [q pid]
    (\(a,b,c) -> a) <$> checkAuthExist pid "comment_id" query


----------------------------------Photo----------------------------------------
photos :: MT m => m ()
photos = withParam "photos" $ do
    ParamEq (Int cid) <- Cache.getParam "content_id"
    delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q cid]
    Insert.photos

----------------------------------Common---------------------------------------
--упорядочить эти функции
updates :: MError m => ParamsMap Param -> [BSName] -> m Query
updates params names = DB.concat "," <$> mapMaybeM helper names where
    helper :: MError m => BSName -> m (Maybe Query)
    helper name = upd (q name) (params ! name)
    upd :: MError m => Query -> Param -> m (Maybe Query)
    upd "pass" (ParamEq v) = return . Just $ template 
        [sql|pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [p $ params ! "login", val v]
    upd field (ParamEq v) = return . Just $ template [sql|{0} = {1}|] [field, val v]
    upd field ParamNo = return Nothing
    upd field ParamNull = return . Just $ template [sql|{0} = null|] [field]
    upd field param = Error.throw $ DevError $ template "Неверный шаблон {0} в функции updates" [show param]

checkAuthExist :: MT m => Int -> BSName -> Query ->  m (Int, Int, Int)
checkAuthExist pid name query = do
    exist <- DB.query_ query
    case exist of
        [] -> Error.throw $ DBError  (template "Указан несуществующий параметр {0}: {1}" [show name, show pid])
        [(uid, aid, cid)] -> do
            auth <- Cache.getAuth
            case auth of
                AuthNo                            -> Error.throw authErrorDefault
                AuthAdmin _                       -> return (uid, aid, cid) --админ может РЕДАКТИРОВАТЬ все публикации (модерация)
                AuthUser authuid | uid == authuid -> return (uid, aid, cid) --юзер может РЕДАКТИРОВАТЬ только своё
                _                                 -> Error.throw authErrorWrong
        res -> Error.throw $ DevError $ template "Неверный результат запроса {0} в функции checkAuthExist" [show res]

withParam :: MCache m => BSName -> m() -> m()
withParam name t = do
    param <- Cache.getParam name
    case param of
        ParamNo -> return ()
        _       -> t
