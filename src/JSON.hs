{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module JSON where

import qualified Row 
import qualified Select 
import Database.PostgreSQL.Simple.Time
import Data.Text (pack, Text(..))
import Types --(Path, E)

import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Error
import Control.Monad.Except
import Common
import Data.List
import Control.Monad.Trans.Except



data Post = Post {
    postId :: Int,
    postContent :: Content
} deriving (Show, Generic)
instance ToJSON Post

data Content = Content {
    contentId :: Int,
    contentAuthor :: Author,
    contentName :: String,
    contentCreationDate :: Date,
    contentCategory :: Category,
    contentText :: Text,
    contentPhoto :: Path,
    contentTags :: [Tag]
} deriving (Show, Generic)
instance ToJSON Content

data Author = Author {
    authorId :: Int,
    authorUser :: Row.User,
    authorDescription :: String
} deriving (Show, Generic)
instance ToJSON Author

data Category = Category{
    categoryId :: Int,
    parent :: Maybe Category,
    categoryName :: String
} deriving (Show, Generic)
instance ToJSON Category

type User = Row.User
type Tag = Row.Tag

-- getPosts:: [Select.Post] -> [JSON.Post] 
-- getPosts = undefined




----------EVALUATE--------------
--такой ответ кажется избыточный, но по условиям УЧЕБНОГО проекта требуются именно вложенные сущности.

--evaluate from 'select' types to 'json' types
evalCategories :: [Select.Category] -> Except E [Category]
evalCategories rcs = mapM (evalCategory [] rcs . Row.categoryId) rcs

evalCategory :: [Int] -> [Select.Category] -> Int -> Except E Category
evalCategory  childs rcs categoryId = do
    if categoryId `elem` childs then do
        throwE . DBError $ template "Обнаружена циклическая категория {0}, которая является своим же родителем" [show categoryId]
    else do
        let mrc = find (\(Row.Category cid _ _) -> cid == categoryId) rcs --двух категорий с одинаковым первичным ключом быть не может. Но может быть Nothing
        case mrc of 
            Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Row.Category categoryId mparentId name) -> do
                case mparentId of
                    Nothing -> return $ Category categoryId Nothing name  
                    Just parentId -> do
                        parentCategory <- evalCategory (categoryId:childs) rcs parentId 
                        return $ Category categoryId (Just parentCategory) name 

evalPosts :: [Category] -> [Select.Post] -> Except E [Post]
evalPosts cs = mapM (evalPost cs)

evalPost :: [Category] -> Select.Post -> Except E Post
evalPost cs (Select.Post rpost rcontent rauthor user mtag)  = do
    let postId = Row.postId rpost
    let categoryId = Row.contentCategoryId rcontent
    let mcategory = find (\(Category cid _ _) -> cid == categoryId) cs
    case mcategory of
        Nothing -> do
            throwE . DBError $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
        Just category -> do
            let author = turnAuthor rauthor user
            let postContent = turnContent rcontent author category (maybeToList mtag)
            return Post {..}

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

----------TURN--------------
--turn from 'row' types to 'json' types
turnContent ::  Row.Content -> Author -> Category -> [Tag]-> Content
turnContent (Row.Content a _ c d _ f g) author category tags  = Content a author c d category f g tags

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author a _ c) user = Author a user c 



    
