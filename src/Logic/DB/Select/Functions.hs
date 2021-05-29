module Logic.DB.Select.Functions where

import Common.Functions (Template (template))
import Data.Map as M ((!))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL ()
import Interface.Class (MDB, MTrans)
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (toQuery, whereAll, whereAllM, (<+>), (<<+>>))
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row
import Logic.DB.Select.Internal
  ( authUserIdParam,
    authorsQuery,
    categoriesQuery,
    commentsQuery,
    cond,
    pagination,
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
import Logic.DB.Select.Types
  ( Author,
    Category,
    Comment,
    Draft,
    Migration,
    Post,
    Tag,
    User,
  )

-----------------------------Migration----------------------------------------------
allMigrations :: MDB m => m [Migration]
allMigrations = DB.query selectMigrationsQuery

-----------------------------User----------------------------------------------
user :: MDB m => Int -> m (Maybe User)
user pid = listToMaybe <$> DB.query qu
  where
    qu = selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [toQuery pid]

users :: MTrans m => m [User]
users = DB.query =<< usersQuery

-----------------------------Author--------------------------------------------
author :: MDB m => Int -> m (Maybe Author)
author pid = listToMaybe <$> DB.query qu
  where
    qu = selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [toQuery pid]

authors :: MTrans m => m [Author]
authors = DB.query =<< authorsQuery

-----------------------------Category------------------------------------------
category :: MDB m => Int -> m (Maybe Category)
category pid = listToMaybe <$> DB.query qu
  where
    qu = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [toQuery pid]

categories :: MTrans m => m [Category]
categories = DB.query =<< categoriesQuery

-- * All categories without pagination are needed to evaluate parent categories

allCategories :: MDB m => m [Category]
allCategories = DB.query selectCategoriesQuery

-----------------------------Draft---------------------------------------------
draft :: MTrans m => Int -> m [Draft]
draft pid = do
  paramUserId <- authUserIdParam
  conditions <-
    sequenceA
      [ cond [sql|drafts.id|] $ Cache.ParamEq (Cache.Int pid),
        cond [sql|users.id|] paramUserId
      ]
  let qu = selectDraftsQuery `whereAll` conditions
  DB.query qu

drafts :: MTrans m => m [Draft]
drafts = do
  paramUserId <- authUserIdParam
  let conditions = [cond [sql|users.id|] paramUserId]
  qu <- selectDraftsQuery `whereAllM` conditions <<+>> pagination
  DB.query qu

-----------------------------Post----------------------------------------------
post :: MDB m => Int -> m [Post]
post pid = DB.query qu
  where
    qu = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [toQuery pid]

posts :: MTrans m => m [Post]
posts = DB.query =<< postsQuery

-----------------------------Tag-----------------------------------------------
tag :: MDB m => Int -> m (Maybe Tag)
tag pid = listToMaybe <$> DB.query qu
  where
    qu = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [toQuery pid]

tags :: MTrans m => m [Tag]
tags = DB.query =<< tagsQuery

-----------------------------Comment-------------------------------------------
comments :: MTrans m => Int -> m [Comment]
comments postId = DB.query =<< commentsQuery postId