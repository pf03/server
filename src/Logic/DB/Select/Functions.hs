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
      selectMigrationsQuery,
      paramToCondition )
import Logic.DB.Select.Types
    ( Author, Category, Comment, Draft, Migration, Post, Tag, User )

-----------------------------Migration----------------------------------------------
allMigrations :: MDB m => m [Migration]
allMigrations = DB.query selectMigrationsQuery

-----------------------------User----------------------------------------------
user :: MDB m => Int -> m (Maybe User)
user paramId = listToMaybe <$> DB.query query
  where
    query = selectUsersQuery <+> template [sql|WHERE users.id = {0}|] [toQuery paramId]

users :: MTrans m => m [User]
users = DB.query =<< usersQuery

-----------------------------Author--------------------------------------------
author :: MDB m => Int -> m (Maybe Author)
author paramId = listToMaybe <$> DB.query query
  where
    query = selectAuthorsQuery <+> template [sql|WHERE authors.id = {0}|] [toQuery paramId]

authors :: MTrans m => m [Author]
authors = DB.query =<< authorsQuery

-----------------------------Category------------------------------------------
category :: MDB m => Int -> m (Maybe Category)
category paramId = listToMaybe <$> DB.query query
  where
    query = selectCategoriesQuery <+> template [sql|WHERE categories.id = {0}|] [toQuery paramId]

categories :: MTrans m => m [Category]
categories = DB.query =<< categoriesQuery

-- * All categories without pagination are needed to evaluate parent categories

allCategories :: MDB m => m [Category]
allCategories = DB.query selectCategoriesQuery

-----------------------------Draft---------------------------------------------
draft :: MTrans m => Int -> m [Draft]
draft paramId = do
  paramUserId <- authUserIdParam
  conditions <-
    sequenceA
      [ paramToCondition [sql|drafts.id|] $ Cache.ParamEq (Cache.Int paramId),
        paramToCondition [sql|users.id|] paramUserId
      ]
  let query = selectDraftsQuery `whereAll` conditions
  DB.query query

drafts :: MTrans m => m [Draft]
drafts = do
  paramUserId <- authUserIdParam
  let conditions = [paramToCondition [sql|users.id|] paramUserId]
  query <- selectDraftsQuery `whereAllM` conditions <<+>> pagination
  DB.query query

-----------------------------Post----------------------------------------------
post :: MDB m => Int -> m [Post]
post paramId = DB.query query
  where
    query = selectPostsQuery <+> template [sql|WHERE posts.id = {0}|] [toQuery paramId]

posts :: MTrans m => m [Post]
posts = DB.query =<< postsQuery

-----------------------------Tag-----------------------------------------------
tag :: MDB m => Int -> m (Maybe Tag)
tag paramId = listToMaybe <$> DB.query query
  where
    query = selectTagsQuery <+> template [sql|WHERE tags.id = {0}|] [toQuery paramId]

tags :: MTrans m => m [Tag]
tags = DB.query =<< tagsQuery

-----------------------------Comment-------------------------------------------
comments :: MTrans m => Int -> m [Comment]
comments postId = DB.query =<< commentsQuery postId