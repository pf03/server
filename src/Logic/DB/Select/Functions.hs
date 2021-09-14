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
selectAllMigrations = DB.dbQuery_ migrationsQuery

-----------------------------User----------------------------------------------
selectUser :: MDB m => Int -> m (Maybe User)
selectUser userId = listToMaybe <$> DB.dbQuery_ (userQuery userId)

selectUsers :: MTrans m => m [User]
selectUsers = DB.dbQuery_ =<< filteredUsersQuery

-----------------------------Author--------------------------------------------
selectAuthor :: MDB m => Int -> m (Maybe Author)
selectAuthor authorId = listToMaybe <$> DB.dbQuery_ (authorQuery authorId)

selectAuthors :: MTrans m => m [Author]
selectAuthors = DB.dbQuery_ =<< filteredAuthorsQuery

-----------------------------Category------------------------------------------
selectCategory :: MDB m => Int -> m (Maybe Category)
selectCategory categoryId = listToMaybe <$> DB.dbQuery_ (categoryQuery categoryId)

selectCategories :: MTrans m => m [Category]
selectCategories = DB.dbQuery_ =<< filteredCategoriesQuery

-- * All categories without pagination are needed to evaluate parent categories

selectAllCategories :: MDB m => m [Category]
selectAllCategories = DB.dbQuery_ categoriesQuery

-----------------------------Draft---------------------------------------------
selectDraft :: MTrans m => Int -> m [Content]
selectDraft contentId = DB.dbQuery_ =<< draftQuery contentId

selectDrafts :: MTrans m => m [Content]
selectDrafts = DB.dbQuery_ =<< filteredDraftsQuery

-----------------------------Post----------------------------------------------
selectPost :: MDB m => Int -> m [Content]
selectPost contentId = DB.dbQuery_ $ postQuery contentId

selectPosts :: MTrans m => m [Content]
selectPosts = DB.dbQuery_ =<< filteredPostsQuery

-----------------------------Tag-----------------------------------------------
selectTag :: MDB m => Int -> m (Maybe Tag)
selectTag tagId = listToMaybe <$> DB.dbQuery_ (tagQuery tagId)

selectTags :: MTrans m => m [Tag]
selectTags = DB.dbQuery_ =<< filteredTagsQuery

-----------------------------Comment-------------------------------------------
selectComments :: MTrans m => Int -> m [Comment]
selectComments postId = DB.dbQuery_ =<< filteredCommentsQuery postId