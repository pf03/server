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
  ( APIType (Author, Category, Content, Tag, User),
    Auth (AuthAdmin, AuthNo, AuthUser),
    Param (ParamAll, ParamEq, ParamNo, ParamNull),
    ParamsMap,
    QueryType (Insert),
    Val (Int),
  )
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (concatWith, toQuery)
import qualified Interface.MError.Exports as Error
import Logic.DB.Insert (checkExist, rowEither)
import qualified Logic.DB.Insert as Insert
import Logic.DB.Select.Templates
  ( paramToQuery,
    valListToQuery,
    valToQuery,
  )

----------------------------------User-----------------------------------------
updateUser :: MTrans m => Int -> m ()
updateUser paramId = do
  _ <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  -- Login is required to generate new password
  [Only login] <- DB.query [sql|SELECT users.user_login FROM users WHERE users.id = {0}|] [toQuery paramId]
  params <- Cache.addStrParam "user_login" login
  DB.updateM
    User
    [sql|UPDATE users SET {0} WHERE id = {1}|]
    [updates params ["first_name", "last_name", "avatar", "pass"], return $ toQuery paramId]

----------------------------------Author---------------------------------------
updateAuthor :: MTrans m => Int -> m ()
updateAuthor paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM authors WHERE authors.id = {0}|]
  checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  DB.updateM
    Author
    [sql|UPDATE authors SET {0} WHERE id = {1}|]
    [updates params ["user_id", "description"], return $ toQuery paramId]

----------------------------------Category-------------------------------------
updateCategory :: MTrans m => Int -> m ()
updateCategory paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.parent_id = {0}|]
  DB.updateM
    Category
    [sql|UPDATE categories SET {0} WHERE id = {1}|]
    [updates params ["parent_id", "category_name"], return $ toQuery paramId]

----------------------------------Tag------------------------------------------
updateTag :: MTrans m => Int -> m ()
updateTag paramId = do
  params <- Cache.addIdParam "id" paramId
  checkExist "id" [sql|SELECT 1 FROM tags WHERE tags.id = {0}|]
  DB.updateM
    Tag
    [sql|UPDATE tags SET tag_name = {0} WHERE id = {1}|]
    [paramToQuery $ params ! "tag_name", return $ toQuery paramId]

updateTagToContent :: MTrans m => Action -> m ()
updateTagToContent Check = withParam "tag_id" $ Insert.insertTagToContent Check
updateTagToContent Execute = withParam "tag_id" $ do
  ParamEq (Int cid) <- Cache.getParam "content_id"
  DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [toQuery cid]
  Insert.insertTagToContent Execute

----------------------------------Content----------------------------------------

-- | Content existence check with authorization for update and delete requests

-- * Authentication as deleted user will fail. The deleted author is bound to the deleted

-- user. Thus, posts with deleted authors and users will be able to edit
-- only admin

checkAuthExistContent :: MTrans m => Bool -> Int -> m ParamsMap
checkAuthExistContent isDraft paramId = do
  let query =
        template
          [sql|

        SELECT users.id, authors.id, contents.id FROM contents
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        WHERE contents.id = {0} 
        AND contents.is_draft={1}|]
          [toQuery paramId, if isDraft then [sql|TRUE|] else [sql|FALSE|]]
  let paramName = if isDraft then "draft_id" else "post_id"
  (userId, authorId, contentId) <- checkAuthExist paramId paramName query
  Cache.addIdParam_ "user_id" userId
  Cache.addIdParam_ "author_id" authorId
  Cache.addIdParam "content_id" contentId

----------------------------------Draft----------------------------------------
updateDraft :: MTrans m => Int -> m ()
updateDraft paramId = do
  _ <- checkAuthExistContent True paramId
  params <- Cache.getParams
  checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  updateTagToContent Check
  ParamEq (Int contentId) <- Cache.getParam "content_id"
  DB.updateM
    Content
    [sql|UPDATE contents SET {0} WHERE id = {1}|]
    [updates params ["content_name", "category_id", "content_text", "main_photo", "photos"], return $ toQuery contentId]
  updateTagToContent Execute

----------------------------------Post-----------------------------------------
updatePost :: MTrans m => Int -> m ()
updatePost paramId = do
  params <- checkAuthExistContent False paramId
  when (params ! "author_id" == ParamEq (Int 1)) $
    Error.throwDB "Unable to create draft from deleted author with id = 1" []
  checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  Insert.insertTagToContent Check
  [Only contentId] <-
    DB.queryM
      [sql|INSERT into contents (author_id, content_name, creation_date, category_id, content_text, main_photo, photos, is_draft, post_id) values {0} RETURNING id|]
      [ rowEither
          params
          [ Left "author_id",
            Left "content_name",
            Right [sql|current_date|],
            Left "category_id",
            Left "content_text",
            Left "main_photo",
            Left "photos",
            Right [sql|TRUE|],
            Right $ toQuery paramId
          ]
      ]
  Cache.addChanged Insert Content 1
  Cache.addIdParam_ "content_id" contentId -- update param
  Insert.insertTagToContent Execute

----------------------------------Comment--------------------------------------

-- * In common checkAuthExist returns 3 parameters. This function returns only userId

checkAuthExistComment :: MTrans m => Int -> m Int
checkAuthExistComment paramId = do
  let query = template [sql| SELECT user_id, 0, 0 FROM comments WHERE id = {0}|] [toQuery paramId]
  (\(a, _, _) -> a) <$> checkAuthExist paramId "comment_id" query

----------------------------------Common---------------------------------------
updates :: MError m => ParamsMap -> [BSName] -> m Query
updates params names = concatWith "," <$> mapMaybeM helper names
  where
    helper :: MError m => BSName -> m (Maybe Query)
    helper name = upd (toQuery name) (params ! name)
    upd :: MError m => Query -> Param -> m (Maybe Query)
    upd "pass" (ParamEq val) = do
      login <- paramToQuery $ params ! "user_login"
      return . Just $ template [sql|pass = md5 (CONCAT_WS(' ', {0}, {1}))|] [login, valToQuery val]
    upd field (ParamEq val) = return . Just $ template [sql|{0} = {1}|] [field, valToQuery val]
    upd _ ParamNo = return Nothing
    upd field ParamNull = return . Just $ template [sql|{0} = null|] [field]
    upd field (ParamAll list) = return . Just $ template [sql|{0} = {1}|] [field, valListToQuery list]
    upd _ param = Error.throw $ Error.patError "Update.updates" param

checkAuthExist :: MTrans m => Int -> BSName -> Query -> m (Int, Int, Int)
checkAuthExist paramId name query = do
  exist <- DB.query_ query
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
