module Logic.DB.Update where

import Common.Functions (Template (template), mapMaybeM)
import Common.Types (Action (..), BSName)
import Control.Monad.Identity (when)
import Data.Map as M ((!))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Only (Only), Query)
import Interface.Class (MCache, MError, MTrans)
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types
  ( APIType (Author, Category, Content, Draft, Photo, Tag, User),
    Auth (AuthAdmin, AuthNo, AuthUser),
    Param (ParamEq, ParamNo, ParamNull),
    ParamsMap,
    QueryType (Insert),
    Val (Int),
  )
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (concatWith, toQuery, whereAllM)
import qualified Interface.MError.Exports as Error
import Logic.DB.Insert as Insert (checkExist, photos, rowEither, tagToContent)
import Logic.DB.Select.Templates
  ( paramToCondition,
    paramToQuery,
    valToQuery,
  )

----------------------------------User-----------------------------------------
user :: MTrans m => Int -> m ()
user paramId = do
  _ <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  -- Login is required to generate new password
  [Only login] <- DB.query $ template [sql|SELECT users.login FROM users WHERE users.id = {0}|] [toQuery paramId]
  params <- Cache.addStrParam "login" login
  DB.updateM User [sql|UPDATE users SET {0} WHERE id = {1}|]
    [updates params ["first_name", "last_name", "avatar", "pass"], return $ toQuery paramId]

----------------------------------Author---------------------------------------
author :: MTrans m => Int -> m ()
author paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
  checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  DB.updateM Author [sql|UPDATE authors SET {0} WHERE id = {1}|]
    [updates params ["user_id", "description"], return $ toQuery paramId]

----------------------------------Category-------------------------------------
category :: MTrans m => Int -> m ()
category paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.parent_id = {0}|]
  DB.updateM Category [sql|UPDATE categories SET {0} WHERE id = {1}|]
    [updates params ["parent_id", "category_name"], return $ toQuery paramId]

----------------------------------Tag------------------------------------------
tag :: MTrans m => Int -> m ()
tag paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
  DB.updateM Tag [sql|UPDATE tags SET name = {0} WHERE id = {1}|]
    [paramToQuery $ params ! "name", return $ toQuery paramId]

tagToContent :: MTrans m => Action -> m ()
tagToContent Check = withParam "tag_id" $ Insert.tagToContent Check
tagToContent Execute = withParam "tag_id" $ do
  ParamEq (Int cid) <- Cache.getParam "content_id"
  DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [toQuery cid]
  Insert.tagToContent Execute

----------------------------------Draft----------------------------------------
draft :: MTrans m => Int -> m ()
draft paramId = do
  _ <- checkAuthExistDraft paramId
  params <- Cache.getParams
  checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  Logic.DB.Update.tagToContent Check
  ParamEq (Int contentId) <- Cache.getParam "content_id"
  DB.updateM Content [sql|UPDATE contents SET {0} WHERE id = {1}|]
    [updates params ["name", "category_id", "text", "photo"], return $ toQuery contentId]
  Logic.DB.Update.tagToContent Execute
  Logic.DB.Update.photos

-- | Draft existence check with authorization for update and delete requests
checkAuthExistDraft :: MTrans m => Int -> m ParamsMap
checkAuthExistDraft paramId = do
  qu <-
    [sql|
        SELECT users.id, authors.id, contents.id FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |]
      `whereAllM` [paramToCondition [sql|drafts.id|] $ ParamEq (Int paramId)]
  (uid, aid, cid) <- checkAuthExist paramId "draft_id" qu
  Cache.addIdParam_ "id" paramId
  Cache.addIdParam_ "user_id" uid
  Cache.addIdParam_ "author_id" aid
  Cache.addIdParam "content_id" cid

----------------------------------Post-----------------------------------------
post :: MTrans m => Int -> m ()
post paramId = do
  params <- checkAuthExistPost paramId
  when (params ! "author_id" == ParamEq (Int 1)) $
    Error.throwDB "Unable to create draft from deleted author with id = 1" []
  checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  Insert.tagToContent Check
  [Only contentId] <-
    DB.queryM
      [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|]
      [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]]
  Cache.addChanged Insert Content 1
  DB.insert
    Draft
    [sql|INSERT into drafts (content_id, post_id) values ({0}, {1})|]
    [toQuery contentId, toQuery paramId]
  Cache.addIdParam_ "content_id" contentId -- update param
  Insert.tagToContent Execute
  Insert.photos

-- * Deleted user authentication will fail. The deleted author is bound to the deleted

-- user. Thus, posts with deleted authors and users will be able to edit
-- only admin
checkAuthExistPost :: MTrans m => Int -> m ParamsMap
checkAuthExistPost paramId = do
  query <-
    [sql|
        SELECT users.id, authors.id, contents.id FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    |]
      `whereAllM` [paramToCondition [sql|posts.id|] $ ParamEq (Int paramId)]
  (userId, authorId, contentId) <- checkAuthExist paramId "post_id" query
  Cache.addIdParam_ "id" paramId
  Cache.addIdParam_ "user_id" userId
  Cache.addIdParam_ "author_id" authorId
  Cache.addIdParam "content_id" contentId

----------------------------------Comment--------------------------------------

-- * In common checkAuthExist returns 3 parameters. This function returns only userId

checkAuthExistComment :: MTrans m => Int -> m Int
checkAuthExistComment paramId = do
  let query = template [sql| SELECT user_id, 0, 0 FROM comments WHERE id = {0}|] [toQuery paramId]
  (\(a, _, _) -> a) <$> checkAuthExist paramId "comment_id" query

----------------------------------Photo----------------------------------------
photos :: MTrans m => m ()
photos = withParam "photos" $ do
  ParamEq (Int contentId) <- Cache.getParam "content_id"
  DB.delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [toQuery contentId]
  Insert.photos

----------------------------------Common---------------------------------------
updates :: MError m => ParamsMap -> [BSName] -> m Query
updates params names = concatWith "," <$> mapMaybeM helper names
  where
    helper :: MError m => BSName -> m (Maybe Query)
    helper name = upd (toQuery name) (params ! name)
    upd :: MError m => Query -> Param -> m (Maybe Query)
    upd "pass" (ParamEq val) = do
      login <- paramToQuery $ params ! "login"
      return . Just $ template [sql|pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [login, valToQuery val]
    upd field (ParamEq val) = return . Just $ template [sql|{0} = {1}|] [field, valToQuery val]
    upd _ ParamNo = return Nothing
    upd field ParamNull = return . Just $ template [sql|{0} = null|] [field]
    upd _ param = Error.throw $ Error.patError "Update.updates" param

checkAuthExist :: MTrans m => Int -> BSName -> Query -> m (Int, Int, Int)
checkAuthExist paramId name query = do
  exist <- DB.query query
  case exist of
    [] -> Error.throwDB "Entity {0} = {1} is not exist" [show name, show paramId]
    [(userId, authorId, contentId)] -> do
      a <- Cache.getAuth
      case a of
        AuthNo -> Error.throw Error.authErrorDefault
        AuthAdmin _ -> return (userId, authorId, contentId) -- admin can EDIT all contents (moderation)
        AuthUser authUserId | userId == authUserId -> return (userId, authorId, contentId) -- user can EDIT only his own contents
        _ -> Error.throw Error.authErrorWrong
    res -> Error.throw $ Error.DevError $ template "Wrong query result {0} in function Update.checkAuthExist" [show res]

withParam :: MCache m => BSName -> m () -> m ()
withParam name m = do
  param <- Cache.getParam name
  case param of
    ParamNo -> return ()
    _ -> m
