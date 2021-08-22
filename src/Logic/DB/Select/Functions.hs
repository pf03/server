module Logic.DB.Select.Functions where

import Data.Maybe (listToMaybe)
import Interface.Class (MDB, MTrans)
import qualified Interface.MDB.Exports as DB
import Logic.DB.Select.Internal
  ( authorQuery,
    categoriesQuery,
    categoryQuery,
    draftQuery,
    filteredAuthorsQuery,
    filteredCategoriesQuery,
    filteredCommentsQuery,
    filteredDraftsQuery,
    filteredPostsQuery,
    filteredTagsQuery,
    filteredUsersQuery,
    migrationsQuery,
    postQuery,
    tagQuery,
    userQuery,
  )
import Logic.DB.Select.Types (Author, Category, Comment, Content, Migration, Tag, User)

-----------------------------Migration----------------------------------------------
selectAllMigrations :: MDB m => m [Migration]
selectAllMigrations = DB.query migrationsQuery

-----------------------------User----------------------------------------------
selectUser :: MDB m => Int -> m (Maybe User)
selectUser userId = listToMaybe <$> DB.query (userQuery userId)

selectUsers :: MTrans m => m [User]
selectUsers = DB.query =<< filteredUsersQuery

-----------------------------Author--------------------------------------------
selectAuthor :: MDB m => Int -> m (Maybe Author)
selectAuthor authorId = listToMaybe <$> DB.query (authorQuery authorId)

selectAuthors :: MTrans m => m [Author]
selectAuthors = DB.query =<< filteredAuthorsQuery

-----------------------------Category------------------------------------------
selectCategory :: MDB m => Int -> m (Maybe Category)
selectCategory categoryId = listToMaybe <$> DB.query (categoryQuery categoryId)

selectCategories :: MTrans m => m [Category]
selectCategories = DB.query =<< filteredCategoriesQuery

-- * All categories without pagination are needed to evaluate parent categories

selectAllCategories :: MDB m => m [Category]
selectAllCategories = DB.query categoriesQuery

-----------------------------Draft---------------------------------------------
selectDraft :: MTrans m => Int -> m [Content]
selectDraft contentId = DB.query =<< draftQuery contentId

selectDrafts :: MTrans m => m [Content]
selectDrafts = DB.query =<< filteredDraftsQuery

-----------------------------Post----------------------------------------------
selectPost :: MDB m => Int -> m [Content]
selectPost contentId = DB.query $ postQuery contentId

selectPosts :: MTrans m => m [Content]
selectPosts = DB.query =<< filteredPostsQuery

-----------------------------Tag-----------------------------------------------
selectTag :: MDB m => Int -> m (Maybe Tag)
selectTag tagId = listToMaybe <$> DB.query (tagQuery tagId)

selectTags :: MTrans m => m [Tag]
selectTags = DB.query =<< filteredTagsQuery

-----------------------------Comment-------------------------------------------
selectComments :: MTrans m => Int -> m [Comment]
selectComments postId = DB.query =<< filteredCommentsQuery postId