module Logic.DB.Delete
    ( user
    , author
    , post
    , draft
    , comment
    , category
    , tag
    ) where

-- Our modules
import           Common.Misc
import           Interface.Cache                  as Cache
import           Interface.DB                     as DB
import           Interface.Error                  as Error
import qualified Logic.DB.Update                  as Update

-- Other modules
import           Control.Monad.Identity           (when)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types as SQL (Query)

-- | 4 cases for delete
-- 1. The deleted entity should be replaced with the default value. Used for users and authors
-- 2. Cascade delete along with binded entities. Used for posts and drafts.
-- For tags, the tag and all tag bindings to contents should be deleted
-- 3. Delete strictly by condition, if nothing is binded to this entity.
-- First you should to edit or delete binded entities, and then continue deletion.
-- Used for categories.
-- 4. Simple deletion if nothing depends on the entity. Used for comments

-----------------------------Public functions----------------------------------
user :: MT m => Int -> m ()
user pid = do
    when (pid == 1) $ Error.throw $ DBError "Unable to delete default user with id = 1"
    when (pid == 2) $ Error.throw $ DBError "Unable to delete admin with id = 2"
    DB.update Author [sql|UPDATE authors SET user_id = 1 WHERE user_id = {0}|] [q pid]
    DB.update Comment [sql|UPDATE comments SET user_id = 1 WHERE user_id = {0}|] [q pid]
    DB.delete User [sql|DELETE FROM users WHERE id = {0}|] [q pid]

author :: MT m => Int -> m ()
author pid = do
    when (pid == 1) $ Error.throw $ DBError "Unable to delete default author with id = 1"
    DB.update Content [sql|UPDATE contents SET author_id = 1 WHERE author_id = {0}|] [q pid]
    DB.delete Author [sql|DELETE FROM authors WHERE id = {0}|] [q pid]

post :: MT m => Int -> m ()
post pid = do
    _ <- Update.checkAuthExistPost pid
    ParamEq (Int cid) <- Cache.getParam "content_id"
    DB.delete Post [sql|DELETE FROM posts WHERE id = {0}|] [q pid]
    DB.delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q cid]
    DB.delete Draft [sql|DELETE FROM drafts WHERE post_id = {0}|] [q pid]
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q cid]
    DB.delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q cid]
    DB.delete Comment [sql|DELETE FROM comments WHERE post_id = {0}|] [q pid]

draft :: MT m => Int -> m ()
draft pid = do
    _ <- Update.checkAuthExistDraft pid
    ParamEq (Int cid) <- Cache.getParam "content_id"
    DB.delete Draft [sql|DELETE FROM drafts WHERE id = {0}|] [q pid]
    DB.delete Content [sql|DELETE FROM contents WHERE id = {0}|] [q cid]
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q cid]
    DB.delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q cid]

comment :: MT m => Int -> m ()
comment pid = do
    _ <- Update.checkAuthExistComment pid
    DB.delete Comment [sql|DELETE FROM comments WHERE id = {0}|] [q pid]

category :: MT m => Int -> m ()
category pid = do
    checkNotExist pid "category" "child categories" $ template [sql|
        SELECT id, category_name FROM categories
        WHERE categories.parent_id = {0}
    |] [q pid]

    checkNotExist pid "category" "drafts" $ template [sql|
        SELECT drafts.id, contents.name FROM drafts
        LEFT JOIN contents ON contents.id = drafts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        WHERE categories.id = {0}
    |] [q pid]

    checkNotExist pid "category" "posts" $ template [sql|
        SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        WHERE categories.id = {0}
    |] [q pid]

    DB.delete Category [sql|DELETE FROM categories WHERE id = {0}|] [q pid]

tag :: MT m => Int -> m ()
tag pid = do
    DB.execute_ [sql|DELETE FROM tags_to_contents WHERE tag_id = {0}|] [q pid]
    DB.delete Tag [sql|DELETE FROM tags WHERE id = {0}|] [q pid]

-----------------------------Public functions----------------------------------
checkNotExist :: MDB m => Int -> String -> String -> Query -> m ()
checkNotExist pid name1 name2 templ = do
    results <- DB.query $ template templ [q pid]
    case results :: [(Int, String)] of
        [] -> return ()
        _ -> Error.throw $ DBError  (template "Unable to delete {0} because the following {1} are binded to it:\n{2}" 
            [name1, name2, showResults]) where
                showResults = concatMap helper results
                helper :: (Int, String) -> String
                helper (pid0, name0) = template "id = {0}, name = {1}\n" [show pid0, name0]
