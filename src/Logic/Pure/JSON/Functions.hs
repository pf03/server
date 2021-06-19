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

-- intenal
_evalCategory :: MError m => [Int] -> [Select.Category] -> Int -> m Category
_evalCategory childs rcs cid = do
  if cid `elem` childs
    then do
      -- The cyclic category will froze our server when generating json, so let's throw out the error
      Error.throwDB "Cyclic category {0} found, which is its own parent" [show cid]
    else do
      let mrc = findById cid rcs -- There can't be two categories with the same primary key. But it can be no one
      case mrc of
        Nothing -> Error.throwDB "Отсутствует категория {0}" [show cid]
        Just (Row.Category _ mparentId name) -> do
          case mparentId of
            Nothing -> return $ Category cid Nothing name
            Just parentId -> do
              parentCategory <- _evalCategory (cid : childs) rcs parentId
              return $ Category cid (Just parentCategory) name

-- Category should not be its own parent
checkCyclicCategory :: MError m => Int -> ParamsMap -> [Select.Category] -> m ()
checkCyclicCategory cid params rcs = do
  case params M.! "parent_id" of
    ParamNo -> return ()
    ParamNull -> return () -- root category
    ParamEq (Int parentId) -> do
      grandParents <- getParents parentId rcs
      when (cid `elem` grandParents) $
        Error.throwDB
          "Category {0} has category {2} in the parent list {1}. Unable to create cyclic category"
          [show parentId, show grandParents, show cid]
    par -> Error.throw $ Error.patError "JSON.checkCyclicCategory" par

getParents :: MError m => Int -> [Select.Category] -> m [Int]
getParents = helper []
  where
    helper :: MError m => [Int] -> Int -> [Select.Category] -> m [Int]
    helper acc cid rcs = do
      let mrc = findById cid rcs
      case mrc of
        Nothing -> Error.throwDB "Category missing {0}" [show cid]
        Just (Row.Category _ mparentId _) -> case mparentId of
          Nothing -> return acc
          Just parentId -> do
            when (parentId `elem` acc) $
              Error.throwDB "Category {0} is its own parent" [show parentId]
            helper ([parentId] <> acc) parentId rcs

getCategoryById :: MError m => Int -> [Category] -> String -> m Category
getCategoryById cid cs err =
  let mcategory = findById cid cs
   in case mcategory of
        Nothing -> do
          Error.throwDB err []
        Just category -> return category

evalDrafts :: MError m => [Category] -> [Select.Draft] -> m [Draft]
evalDrafts cs l = uniteDrafts <$> mapM (evalDraft cs) l

evalPosts :: MError m => [Category] -> [Select.Post] -> m [Post]
evalPosts cs l = unitePosts <$> mapM (evalPost cs) l

evalPost :: MError m => [Category] -> Select.Post -> m Post
evalPost cs (rpost :. rcontent :. rcategory :. rauthor :. user :. _ :. mtag :. mphoto) = do
  let author = turnAuthor rauthor user
  let pid = Row.postId rpost
  let cid = Row.categoryId rcategory
  category <-
    getCategoryById cid cs $
      template "Post {0} belongs to a category that does not exist {1}" [show pid, show cid]
  let content = turnContent rcontent author category (maybeToList mtag) (maybeToList mphoto)
  let post = turnPost rpost content
  return post

evalDraft :: MError m => [Category] -> Select.Draft -> m Draft
evalDraft cs (rdraft :. rcontent :. rcategory :. rauthor :. user :. _ :. mtag :. mphoto) = do
  let author = turnAuthor rauthor user
  let did = Row.draftId rdraft
  let cid = Row.categoryId rcategory
  category <-
    getCategoryById cid cs $
      template "Draft {0} belongs to a category that does not exist {1}" [show did, show cid]
  let content = turnContent rcontent author category (maybeToList mtag) (maybeToList mphoto)
  let draft = turnDraft rdraft content
  return draft

unitePosts :: [Post] -> [Post]
unitePosts = map (modifyPostTags filterById . modifyPostPhotos filterById) . unite appendPost
  where
    appendPost :: Post -> Post -> Post
    appendPost p1 p2 = setPostTags tags . setPostPhotos photos $ p1
      where
        tags = getPostTags p1 <> getPostTags p2
        photos = getPostPhotos p1 <> getPostPhotos p2

uniteDrafts :: [Draft] -> [Draft]
uniteDrafts = map (modifyDraftTags filterById . modifyDraftPhotos filterById) . unite appendDraft
  where
    appendDraft :: Draft -> Draft -> Draft
    appendDraft p1 p2 = setDraftTags tags . setDraftPhotos photos $ p1
      where
        tags = getDraftTags p1 <> getDraftTags p2
        photos = getDraftPhotos p1 <> getDraftPhotos p2

evalAuthor :: Select.Author -> Author
evalAuthor (author :. user) = turnAuthor author user

evalAuthors :: [Select.Author] -> [Author]
evalAuthors = map evalAuthor

evalComments :: [Select.Comment] -> [Comment]
evalComments = map evalComment

evalComment :: Select.Comment -> Comment
evalComment (comment :. _ :. user) = turnComment comment user

-----------------------------Data manipulation----------------------------------
-- Here the JSON.Category type is used, which has already been checked for cyclicity and correctness in JSON.evalCategory

evalParams :: MError m => [Category] -> ParamsMap -> m ParamsMap
evalParams categories = adjustM (`getChildCategories` categories) ParamNo "category_id"