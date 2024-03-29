module Logic.DB.Delete where

import Common.Template (Template (template))
import Control.Monad.Identity (when)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Query)
import Interface.Class (MDB, MTrans)
import Interface.MCache.Types (APIType (Author, Category, Comment, Content, Tag, User))
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (toQuery)
import Interface.MError.Exports
  ( Error (DBError),
    MError (throwServerError),
  )
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Update as Update

-- | 4 cases for delete
-- 1. The deleted entity should be replaced with the default value. Used for users and authors
-- 2. Cascade delete along with bound entities. Used for posts and drafts.
-- For tags, the tag and all tag bindings to contents should be deleted
-- 3. Delete strictly by condition, if nothing is bound to this entity.
-- First you should to edit or delete bound entities, and then continue deletion.
-- Used for categories.
-- 4. Simple deletion if nothing depends on the entity. Used for comments

-----------------------------Public functions----------------------------------
deleteUser :: MTrans m => Int -> m ()
deleteUser paramId = do
  when (paramId == 1) $ throwServerError $ DBError "Unable to delete default user with id = 1"
  when (paramId == 2) $ throwServerError $ DBError "Unable to delete admin with id = 2"
  DB.dbUpdate Author [sql|UPDATE authors SET user_id = 1 WHERE user_id = {0}|] [toQuery paramId]
  DB.dbUpdate Comment [sql|UPDATE comments SET user_id = 1 WHERE user_id = {0}|] [toQuery paramId]
  DB.dbDelete User [sql|DELETE FROM users WHERE id = {0}|] [toQuery paramId]

deleteAuthor :: MTrans m => Int -> m ()
deleteAuthor paramId = do
  when (paramId == 1) $ throwServerError $ DBError "Unable to delete default author with id = 1"
  DB.dbUpdate Content [sql|UPDATE contents SET author_id = 1 WHERE author_id = {0}|] [toQuery paramId]
  DB.dbDelete Author [sql|DELETE FROM authors WHERE id = {0}|] [toQuery paramId]

deletePost :: MTrans m => Int -> m ()
deletePost contentId = do
  _ <- Update.checkAuthExistContent False contentId
  DB.dbDelete Comment [sql|DELETE FROM comments WHERE post_id = {0}|] [toQuery contentId]
  DB.dbExecute_
    [sql|DELETE FROM tags_to_contents WHERE content_id = {0} OR content_id IN
    (SELECT id FROM contents WHERE post_id = {0})|]
    [toQuery contentId] --delete tags binds to post and all its drafts
  DB.dbDelete Content [sql|DELETE FROM contents WHERE post_id = {0}|] [toQuery contentId] -- delete all post drafts
  DB.dbDelete Content [sql|DELETE FROM contents WHERE id = {0}|] [toQuery contentId] -- delete post

deleteDraft :: MTrans m => Int -> m ()
deleteDraft contentId = do
  _ <- Update.checkAuthExistContent True contentId
  DB.dbExecute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [toQuery contentId]
  DB.dbDelete Content [sql|DELETE FROM contents WHERE id = {0}|] [toQuery contentId]

deleteComment :: MTrans m => Int -> m ()
deleteComment paramId = do
  _ <- Update.checkAuthExistComment paramId
  DB.dbDelete Comment [sql|DELETE FROM comments WHERE id = {0}|] [toQuery paramId]

deleteCategory :: MTrans m => Int -> m ()
deleteCategory paramId = do
  checkNotExist paramId "category" "child categories" $
    template
      [sql|
        SELECT id, category_name FROM categories
        WHERE categories.parent_id = {0}
    |]
      [toQuery paramId]

  checkNotExist paramId "category" "drafts" $
    template
      [sql|
        SELECT drafts.id, contents.content_name FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        WHERE categories.id = {0}
    |]
      [toQuery paramId]

  checkNotExist paramId "category" "posts" $
    template
      [sql|
        SELECT posts.id, contents.content_name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        WHERE categories.id = {0}
    |]
      [toQuery paramId]
  DB.dbDelete Category [sql|DELETE FROM categories WHERE id = {0}|] [toQuery paramId]

deleteTag :: MTrans m => Int -> m ()
deleteTag paramId = do
  DB.dbExecute_ [sql|DELETE FROM tags_to_contents WHERE tag_id = {0}|] [toQuery paramId]
  DB.dbDelete Tag [sql|DELETE FROM tags WHERE id = {0}|] [toQuery paramId]

-----------------------------Private functions----------------------------------
checkNotExist :: MDB m => Int -> String -> String -> Query -> m ()
checkNotExist paramId name1 name2 templ = do
  results <- DB.dbQuery templ [toQuery paramId]
  case results :: [(Int, String)] of
    [] -> return ()
    _ ->
      Error.throwDB
        "Unable to delete {0} because the following {1} are bound to it:\n{2}"
        [name1, name2, showResults]
      where
        showResults = concatMap helper results
        helper :: (Int, String) -> String
        helper (paramId0, name0) = template "id = {0}, name = {1}\n" [show paramId0, name0]