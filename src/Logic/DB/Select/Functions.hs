module Logic.DB.Select.Functions where

import Logic.DB.Select.Templates ( paramToCondition )
import Common.Functions (Template (template))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Interface.Class (MDB, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (toQuery, whereAll, whereAllM, (<+>), (<<+>>))
import Logic.DB.Select.Internal
    ( authUserIdParam,
      commentsQuery,
      tagsQuery,
      selectTagsQuery,
      postsQuery,
      selectPostsQuery,
      selectDraftsQuery,
      categoriesQuery,
      selectCategoriesQuery,
      authorsQuery,
      selectAuthorsQuery,
      pagination,
      usersQuery,
      selectUsersQuery,
      selectMigrationsQuery)
import Logic.DB.Select.Types
    ( Author, Category, Comment, Draft, Migration, Post, Tag, User )

-----------------------------Migration----------------------------------------------
selectAllMigrations :: MDB m => m [Migration]
selectAllMigrations = DB.query selectMigrationsQuery

-----------------------------User----------------------------------------------
selectUser :: MDB m => Int -> m (Maybe User)
selectUser paramId = do
  listToMaybe <$> DB.query query
  where
    query = selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [toQuery paramId]

selectUsers :: MTrans m => m [User]
selectUsers = DB.query =<< usersQuery

-----------------------------Author--------------------------------------------
selectAuthor :: MDB m => Int -> m (Maybe Author)
selectAuthor paramId = listToMaybe <$> DB.query query
  where
    query = selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [toQuery paramId]

selectAuthors :: MTrans m => m [Author]
selectAuthors = DB.query =<< authorsQuery

-----------------------------Category------------------------------------------
selectCategory :: MDB m => Int -> m (Maybe Category)
selectCategory paramId = listToMaybe <$> DB.query query
  where
    query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [toQuery paramId]

selectCategories :: MTrans m => m [Category]
selectCategories = DB.query =<< categoriesQuery

-- * All categories without pagination are needed to evaluate parent categories

selectAllCategories :: MDB m => m [Category]
selectAllCategories = DB.query selectCategoriesQuery

-----------------------------Draft---------------------------------------------
selectDraft :: MTrans m => Int -> m [Draft]
selectDraft paramId = do
  paramUserId <- authUserIdParam
  conditions <-
    sequenceA
      [ paramToCondition [sql|drafts.id|] $ Cache.ParamEq (Cache.Int paramId),
        paramToCondition [sql|users.id|] paramUserId
      ]
  let query = selectDraftsQuery `whereAll` conditions
  DB.query query

selectDrafts :: MTrans m => m [Draft]
selectDrafts = do
  paramUserId <- authUserIdParam
  let conditions = [paramToCondition [sql|users.id|] paramUserId]
  query <- selectDraftsQuery `whereAllM` conditions <<+>> pagination
  DB.query query

-----------------------------Post----------------------------------------------
selectPost :: MDB m => Int -> m [Post]
selectPost paramId = DB.query query
  where
    query = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [toQuery paramId]

selectPosts :: MTrans m => m [Post]
selectPosts = DB.query =<< postsQuery

-----------------------------Tag-----------------------------------------------
selectTag :: MDB m => Int -> m (Maybe Tag)
selectTag paramId = listToMaybe <$> DB.query query
  where
    query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [toQuery paramId]

selectTags :: MTrans m => m [Tag]
selectTags = DB.query =<< tagsQuery

-----------------------------Comment-------------------------------------------
selectComments :: MTrans m => Int -> m [Comment]
selectComments postId = DB.query =<< commentsQuery postId