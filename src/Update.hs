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
import qualified State as S
import Error

--редактирование пароля не верно
user :: Int -> T ()
user pid = do
    S.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM users WHERE users.id = {0}|] 
    [Only login] <- query_ $ template [sql|SELECT users.login FROM users WHERE users.id = {0}|] [q pid]  --логин нужен для проверки пароля!!!
    
    params <- S.addStrParam "login" login
    update User [sql|UPDATE users SET {0} WHERE id = {1}|] 
        [updates params ["first_name", "last_name", "avatar", "pass"], q pid]

author :: Int -> T ()
author pid = do
    params <- S.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    update Author [sql|UPDATE authors SET {0} WHERE id = {1}|] 
        [updates params ["user_id", "description"], q pid]

category :: Int -> T ()
category pid = do
    params <- S.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.parent_id = {0}|]
    update Category [sql|UPDATE categories SET {0} WHERE id = {1}|] 
        [updates params ["parent_id", "category_name"], q pid]

draft :: Int-> T ()
draft pid = do
    
    --проверка существования параметров
    checkAuthExistDraft pid
    params <- S.getParams 
    --let allParams = M.insert "id" (ParamEq (Int pid)) params
    --checkExist allParams "id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    --[Only contentId] <- query_ $ template [sql|SELECT content_id FROM drafts WHERE drafts.id = {0}|] [q pid] :: T [Only Int]
    update Content [sql|UPDATE contents SET {0} WHERE id = {1}|] 
        [updates params ["name", "category_id", "text", "photo", "content_id"]]

--проверка существования вместе с авторизацией, для запросов update и delete
checkAuthExistDraft :: Int -> T (ParamsMap Param)
checkAuthExistDraft pid = do
    let query = [sql|
        SELECT users.id, authors.id, contents.id FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |] `whereAll` [cond [sql|drafts.id|] $ ParamEq (Int pid)]
    (uid, aid, cid) <- checkAuthExist pid "draft_id" query
    S.addIdParam "id" pid
    S.addIdParam "user_id" uid
    S.addIdParam "author_id" aid
    S.addIdParam "content_id" cid

post :: Int -> T ()
post pid = do

    params <- checkAuthExistPost pid
    when (params ! "author_id" == ParamEq (Int 1)) $ throwT $ DBError "Невозможно создать черновик от удаленного автора (автора по умолчанию) id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    check Insert.tagToContent
    [Only cid] <- query_ $ template [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|]
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]] :: T[Only Int]
    S.addChanged Insert Content 1
    insert Draft [sql|INSERT into drafts (content_id, post_id) values ({0}, {1})|] 
        [q cid, q pid]
    S.addIdParam "content_id" cid --перезаписываем
    execute Insert.tagToContent
    Insert.photos
    

checkAuthExistPost :: Int -> T (ParamsMap Param) --(Int, Int, Int) --добавляем authorId для запроса --нужно ли тут чтото возвращать или сразу добавлять в парамсы??
checkAuthExistPost pid = do
    --проверка на удаленного пользователя и удаленного автора!!
    --аутентификация удаленным пользователем не пройдет. Удаленный автор привязан к удаленному пользователю.
    --таким образом посты с удаленными авторами и пользователями сможет редактировать только админ
    let query = [sql|
        SELECT users.id, authors.id, contents.id FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |] `whereAll` [cond [sql|posts.id|] $ ParamEq (Int pid)]
    (uid, aid, cid) <- checkAuthExist pid "post_id" query
    S.addIdParam "id" pid
    S.addIdParam "user_id" uid
    S.addIdParam "author_id" aid
    S.addIdParam "content_id" cid
    --return (uid, aid, cid) -- ??

--в общем случае checkAuthExist возвращает три параметра, поэтому здесь небольшой костыль
checkAuthExistComment :: Int -> T Int --возвращаем userId
checkAuthExistComment pid = do
    let query = template [sql| SELECT user_id, 0, 0 FROM comments WHERE id = {0}|] [q pid]
    (\(a,b,c) -> a) <$> checkAuthExist pid "comment_id" query

tag :: Int -> T ()
tag pid = do
    params <- S.addIdParam "id" pid
    checkExist "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
    update Tag [sql|UPDATE tags SET name = {0} WHERE id = {1}|] [p $ params ! "name", q pid]


--упорядочить эти функции
updates :: ParamsMap Param -> [BSName] -> Query
updates params names = Query.concat "," $ mapMaybe helper names where
    helper :: BSName -> Maybe Query
    helper name = upd (q name) (params ! name)

    upd :: Query -> Param -> Maybe Query 
    --upd "pass" (ParamEq v) = Just $ template [sql|pass = md5({0})|] [val v] --костыль для pass --так не работает
    upd "pass" (ParamEq v) = Just $ template [sql|pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [p $ params ! "login", val v] -- логина может и не быть, нужно его получить дополнительно
    upd field (ParamEq v) = Just $ template [sql|{0} = {1}|] [field, val v]
    upd field ParamNo = Nothing 
    upd field ParamNull = Just $ template [sql|{0} = null|] [field]
    upd field param = error $ template "Нет шаблона для {0}" [show param]

-- --админ может РЕДАКТИРОВАТЬ все публикации (модерация)
-- authAuthorIdParam :: T Param
-- authAuthorIdParam = do
--     auth <- S.getAuth
--     paramUserId <- case auth of
--             AuthAdmin userId -> return ParamNo
--             AuthUser userId -> return $ ParamEq (Int userId)
--             _ -> throwT authErrorDefault
    
--     mauthorId <- fromOnly <<$>> listToMaybe  <$> query_ (template [sql|
--     SELECT authors.id FROM authors
--         LEFT JOIN users
--         ON authors.user_id = users.id
--         WHERE users.id = {0}
--     |] [p paramUserId]) :: T (Maybe Int)
--     case mauthorId of
--         Nothing -> throwT $ AuthError "Данная функция доступна только авторам"
--         Just 1 -> throwT $ AuthError "Невозможна аутентификация удаленного автора"
--         Just authorId -> return $ ParamEq (Int authorId)


checkAuthExist :: Int -> BSName -> Query ->  T (Int, Int, Int)
checkAuthExist pid name query = do
    exist <- query_ query
    case exist of
        [] -> throwT $ DBError  (template "Указан несуществующий параметр {0}: {1}" [show name, show pid]) 
        [(uid, aid, cid)] -> do
            auth <- S.getAuth
            case auth of
                AuthNo -> throwT authErrorDefault
                AuthAdmin _ -> return (uid, aid, cid)
                AuthUser authuid | uid == authuid -> return (uid, aid, cid)
                _ -> throwT authErrorWrong