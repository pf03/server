module Logic.Pure.JSON.Functions where

import Common.Functions (adjustM)
import Common.Identifiable (filterById, findById, unite)
import Common.Template (Template (template))
import Control.Monad.Except (when)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Database.PostgreSQL.Simple (type (:.) ((:.)))
import Interface.Class (MError)
import Interface.MCache.Types
  ( Param (ParamEq, ParamNo, ParamNull),
    ParamValue (IntParam),
    ParamsMap,
  )
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row
import qualified Logic.DB.Select.Exports as Select
import Logic.Pure.JSON.Internal
  ( getChildCategories,
    modifyContentTags,
    setContentTags,
    turnAuthor,
    turnComment,
    turnContent,
  )
import Logic.Pure.JSON.Types (Author, Category (Category), Comment, Content (contentTags))

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
    ParamEq (IntParam parentId) -> do
      grandParents <- getParents parentId categories
      when (categoryId `elem` grandParents) $
        Error.throwDB
          "Category {0} has category {2} in the parent list {1}. Unable to create cyclic category"
          [show parentId, show grandParents, show categoryId]
    param -> Error.throwServerError $ Error.patError "JSON.checkCyclicCategory" param

getParents :: MError m => Int -> [Select.Category] -> m [Int]
getParents = loop []
  where
    loop :: MError m => [Int] -> Int -> [Select.Category] -> m [Int]
    loop acc categoryId categories = do
      let mrc = findById categoryId categories
      case mrc of
        Nothing -> Error.throwDB "Category missing {0}" [show categoryId]
        Just (Row.Category _ mParentId _) -> case mParentId of
          Nothing -> return acc
          Just parentId -> do
            when (parentId `elem` acc) $
              Error.throwDB "Category {0} is its own parent" [show parentId]
            loop ([parentId] <> acc) parentId categories

getCategoryById :: MError m => Int -> [Category] -> String -> m Category
getCategoryById categoryId categories err =
  let mCategory = findById categoryId categories
   in case mCategory of
        Nothing -> do
          Error.throwDB err []
        Just category -> return category

evalContents :: MError m => [Category] -> [Select.Content] -> m [Content]
evalContents categories list = uniteContents <$> mapM (evalContent categories) list

evalContent :: MError m => [Category] -> Select.Content -> m Content
evalContent categories (rowContent :. rowCategory :. rowAuthor :. user :. _ :. mTag) = do
  let author = turnAuthor rowAuthor user
  let contentId = Row.contentId rowContent
  let categoryId = Row.categoryId rowCategory
  category <-
    getCategoryById categoryId categories $
      template "Post {0} belongs to a category that does not exist {1}" [show contentId, show categoryId]
  return $ turnContent rowContent author category (maybeToList mTag)

uniteContents :: [Content] -> [Content]
uniteContents = map (modifyContentTags filterById) . unite appendContent
  where
    appendContent :: Content -> Content -> Content
    appendContent content1 content2 = setContentTags (contentTags content1 <> contentTags content2) content1

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