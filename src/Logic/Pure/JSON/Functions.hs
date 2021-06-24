{-# LANGUAGE ExplicitNamespaces #-}

module Logic.Pure.JSON.Functions where

import Logic.Pure.JSON.Types ( Comment, Category(Category), Author, Draft, Post )
import Logic.Pure.JSON.Internal
    ( turnContent,
      turnAuthor,
      turnComment,
      turnPost,
      turnDraft,
      getPostTags,
      getPostPhotos,
      getDraftTags,
      getDraftPhotos,
      setPostTags,
      setPostPhotos,
      modifyPostTags,
      modifyPostPhotos,
      setDraftTags,
      setDraftPhotos,
      modifyDraftTags,
      modifyDraftPhotos,
      getChildCategories,
      unite )
import Common.Identifiable ( filterById, findById )
import Common.Functions ( adjustM, Template(template), maybeToList )
import Control.Monad.Except (when)
import qualified Data.Map as M
import Database.PostgreSQL.Simple (type (:.) ((:.)))
import Interface.Class (MError)
import Interface.MCache.Types
  ( Param (ParamEq, ParamNo, ParamNull),
    ParamsMap,
    Val (Int),
  )
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row
import qualified Logic.DB.Select.Exports as Select

-----------------------------Evaluate------------------------------------------
-- Evaluate from 'Select' types to 'JSON' types

-- * This response seems redundant, but according to the terms of the TRAINING project, required exactly nested entities.

evalCategories :: MError m => [Select.Category] -> [Select.Category] -> m [Category]
evalCategories allCategories = mapM (_evalCategory [] allCategories . Row.categoryId)

evalCategory :: MError m => [Select.Category] -> Select.Category -> m Category
evalCategory allCategories category = _evalCategory [] allCategories (Row.categoryId category)

-- internal
_evalCategory :: MError m => [Int] -> [Select.Category] -> Int -> m Category
_evalCategory children categories categoryId = do
  if categoryId `elem` children
    then do
      -- The cyclic category will froze our server when generating json, so let's throw out the error
      Error.throwDB "Cyclic category {0} found, which is its own parent" [show categoryId]
    else do
      let mCategory = findById categoryId categories -- There can't be two categories with the same primary key. But it can be no one
      case mCategory of
        Nothing -> Error.throwDB "Category {0} missing" [show categoryId]
        Just (Row.Category _ mParentId name) -> do
          case mParentId of
            Nothing -> return $ Category categoryId Nothing name
            Just parentId -> do
              parentCategory <- _evalCategory (categoryId : children) categories parentId
              return $ Category categoryId (Just parentCategory) name

-- Category should not be its own parent
checkCyclicCategory :: MError m => Int -> ParamsMap -> [Select.Category] -> m ()
checkCyclicCategory categoryId params categories = do
  case params M.! "parent_id" of
    ParamNo -> return ()
    ParamNull -> return () -- root category
    ParamEq (Int parentId) -> do
      grandParents <- getParents parentId categories
      when (categoryId `elem` grandParents) $
        Error.throwDB
          "Category {0} has category {2} in the parent list {1}. Unable to create cyclic category"
          [show parentId, show grandParents, show categoryId]
    param -> Error.throw $ Error.patError "JSON.checkCyclicCategory" param

getParents :: MError m => Int -> [Select.Category] -> m [Int]
getParents = helper []
  where
    helper :: MError m => [Int] -> Int -> [Select.Category] -> m [Int]
    helper acc categoryId categories = do
      let mrc = findById categoryId categories
      case mrc of
        Nothing -> Error.throwDB "Category missing {0}" [show categoryId]
        Just (Row.Category _ mParentId _) -> case mParentId of
          Nothing -> return acc
          Just parentId -> do
            when (parentId `elem` acc) $
              Error.throwDB "Category {0} is its own parent" [show parentId]
            helper ([parentId] <> acc) parentId categories

getCategoryById :: MError m => Int -> [Category] -> String -> m Category
getCategoryById categoryId categories err =
  let mCategory = findById categoryId categories
   in case mCategory of
        Nothing -> do
          Error.throwDB err []
        Just category -> return category

evalDrafts :: MError m => [Category] -> [Select.Draft] -> m [Draft]
evalDrafts cs l = uniteDrafts <$> mapM (evalDraft cs) l

evalPosts :: MError m => [Category] -> [Select.Post] -> m [Post]
evalPosts cs l = unitePosts <$> mapM (evalPost cs) l

evalPost :: MError m => [Category] -> Select.Post -> m Post
evalPost categories (rowPost :. rowContent :. rowCategory :. rowAuthor :. user :. _ :. mTag :. mPhoto) = do
  let author = turnAuthor rowAuthor user
  let postId = Row.postId rowPost
  let categoryId = Row.categoryId rowCategory
  category <-
    getCategoryById categoryId categories $
      template "Post {0} belongs to a category that does not exist {1}" [show postId, show categoryId]
  let content = turnContent rowContent author category (maybeToList mTag) (maybeToList mPhoto)
  let post = turnPost rowPost content
  return post

evalDraft :: MError m => [Category] -> Select.Draft -> m Draft
evalDraft categories (rowDraft :. rowContent :. rowCategory :. rowAuthor :. user :. _ :. mTag :. mPhoto) = do
  let author = turnAuthor rowAuthor user
  let draftId = Row.draftId rowDraft
  let categoryId = Row.categoryId rowCategory
  category <-
    getCategoryById categoryId categories $
      template "Draft {0} belongs to a category that does not exist {1}" [show draftId, show categoryId]
  let content = turnContent rowContent author category (maybeToList mTag) (maybeToList mPhoto)
  let draft = turnDraft rowDraft content
  return draft

unitePosts :: [Post] -> [Post]
unitePosts = map (modifyPostTags filterById . modifyPostPhotos filterById) . unite appendPost
  where
    appendPost :: Post -> Post -> Post
    appendPost post1 post2 = setPostTags tags . setPostPhotos photos $ post1
      where
        tags = getPostTags post1 <> getPostTags post2
        photos = getPostPhotos post1 <> getPostPhotos post2

uniteDrafts :: [Draft] -> [Draft]
uniteDrafts = map (modifyDraftTags filterById . modifyDraftPhotos filterById) . unite appendDraft
  where
    appendDraft :: Draft -> Draft -> Draft
    appendDraft draft1 draft2 = setDraftTags tags . setDraftPhotos photos $ draft1
      where
        tags = getDraftTags draft1 <> getDraftTags draft2
        photos = getDraftPhotos draft1 <> getDraftPhotos draft2

evalAuthor :: Select.Author -> Author
evalAuthor (author :. user) = turnAuthor author user

evalAuthors :: [Select.Author] -> [Author]
evalAuthors = map evalAuthor

evalComments :: [Select.Comment] -> [Comment]
evalComments = map evalComment

evalComment :: Select.Comment -> Comment
evalComment (comment :. _ :. user) = turnComment comment user

-----------------------------Data manipulation----------------------------------
-- Here the JSON.Category type is used, which has already been checked for cyclic recurrence and correctness in JSON.evalCategory

evalParams :: MError m => [Category] -> ParamsMap -> m ParamsMap
evalParams categories = adjustM (`getChildCategories` categories) ParamNo "category_id"