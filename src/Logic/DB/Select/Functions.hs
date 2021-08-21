module Logic.DB.Select.Functions where

import Common.Functions (Template (template))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Interface.Class (MDB, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (toQuery, whereAll, whereAllM, (<+>), (<<+>>))
import Logic.DB.Select.Internal
  ( authUserIdParam,
    authorsQuery,
    categoriesQuery,
    commentsQuery,
    draftsQuery,
    postsQuery,
    selectAuthorsQuery,
    selectCategoriesQuery,
    selectDraftsQuery,
    selectMigrationsQuery,
    selectPostsQuery,
    selectTagsQuery,
    selectUsersQuery,
    tagsQuery,
    usersQuery,
  )
import Logic.DB.Select.Templates (paramToCondition)
import Logic.DB.Select.Types (Author, Category, Comment, Content, Migration, Tag, User)

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
selectDraft :: MTrans m => Int -> m [Content]
selectDraft paramId = do
  paramUserId <- authUserIdParam
  conditions <-
    sequenceA
      [ return [sql|contents.is_draft = TRUE|],
        paramToCondition [sql|contents.id|] $ Cache.ParamEq (Cache.Int paramId),
        paramToCondition [sql|users.id|] paramUserId
      ]
  let query = selectDraftsQuery `whereAll` conditions
  DB.query query

selectDrafts :: MTrans m => m [Content]
selectDrafts = DB.query =<< draftsQuery

-----------------------------Post----------------------------------------------
selectPost :: MDB m => Int -> m [Content]
selectPost paramId = do
  conditions <-
    sequenceA
        [ return [sql|contents.is_draft = FALSE|],
          paramToCondition [sql|contents.id|] $ Cache.ParamEq (Cache.Int paramId)
        ]
  let query = selectPostsQuery `whereAll` conditions
  DB.query query

selectPosts :: MTrans m => m [Content]
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