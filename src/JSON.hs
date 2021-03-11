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
import Database.PostgreSQL.Simple
import Class
import qualified Data.Map as M

-- в результирующем json убрать префиксы, и возможно сделать snake_case
data Post = Post {
    postId :: Int,
    postContent :: Content
} deriving (Show, Generic)
instance ToJSON Post
instance Identifiable Post where
    getId = postId

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
instance Identifiable Content where
    getId = contentId

data Author = Author {
    authorId :: Int,
    authorUser :: Row.User,
    authorDescription :: String
} deriving (Show, Generic)
instance ToJSON Author
instance Identifiable Author where
    getId = authorId

data Category = Category{
    categoryId :: Int,
    parent :: Maybe Category,
    categoryName :: String
} deriving (Show, Generic)
instance ToJSON Category
instance Identifiable Category where
    getId = categoryId

data Comment = Comment {
    commentId :: Int,
    commentUser :: User,
    commentCreationDate :: Date,
    commentText :: Text
} deriving (Show, Generic)
instance ToJSON Comment
instance Identifiable Comment where
    getId = commentId

type User = Row.User
type Tag = Row.Tag

-- getPosts:: [Select.Post] -> [JSON.Post] 
-- getPosts = undefined

----------EVALUATE--------------
--такой ответ кажется избыточный, но по условиям УЧЕБНОГО проекта требуются именно вложенные сущности.

--evaluate from 'select' types to 'json' types
-- evalCategories :: [Select.Category] -> Except E [Category]
-- evalCategories rcs = mapM (evalCategory [] rcs . Row.categoryId) rcs

--здесь лучше упорядочить функции!!!!!!!!!!!!!!!!

evalCategories :: [Select.Category] -> [Select.Category] -> Except E [Category]
evalCategories allCategories  = mapM (_evalCategory [] allCategories . Row.categoryId)

evalCategory :: [Select.Category] -> Select.Category -> Except E Category
evalCategory  allCategories category = _evalCategory [] allCategories (Row.categoryId category)

--intenal
_evalCategory :: [Int] -> [Select.Category] -> Int -> Except E Category
_evalCategory  childs rcs categoryId = do
    if categoryId `elem` childs then do
        --циклическая категория повесит наш сервак при формировании json, поэтому выкинем ошибку
        throwE . DBError $ template "Обнаружена циклическая категория {0}, которая является своим же родителем" [show categoryId]
    else do
        let mrc = findById categoryId rcs --двух категорий с одинаковым первичным ключом быть не может. Но может быть Nothing
        case mrc of 
            Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Row.Category categoryId mparentId name) -> do
                case mparentId of
                    Nothing -> return $ Category categoryId Nothing name  
                    Just parentId -> do
                        parentCategory <- _evalCategory (categoryId:childs) rcs parentId 
                        return $ Category categoryId (Just parentCategory) name 

evalPosts :: [Category] -> [Select.Post] -> Except E [Post]
evalPosts cs = mapM (evalPost cs) 

evalUnitedPosts::[Category] -> [Select.Post] -> Except E [Post]
evalUnitedPosts cs l = unitePosts <$> mapM (evalPost cs) l  

--unite equal posts with different tags
unitePosts :: [Post] -> [Post]
unitePosts = foldr helper [] where
    helper :: Post -> [Post] -> [Post]
    helper curPost unitedPosts  = res where 
        curId = getId curPost;
        --munitedPost = findById curId unitedPosts;
        res = if existId curId unitedPosts 
            then updateById curId (uniteTwoPosts curPost) unitedPosts 
            else curPost : unitedPosts
        uniteTwoPosts :: Post -> Post -> Post
        uniteTwoPosts post1 post2 = setPostTags post1 (getPostTags post1 <> getPostTags post2)

-- tag1 = Row.Tag 1 "tag1"
-- tag2 = Row.Tag 2 "tag2"

evalPost :: [Category] -> Select.Post -> Except E Post
evalPost cs (rpost :. rcontent :. _ :.  rauthor :. user :. _ :. mtag)  = do
    let postId = Row.postId rpost
    let categoryId = Row.contentCategoryId rcontent
    let mcategory = findById categoryId cs
    case mcategory of
        Nothing -> do
            throwE . DBError $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
        Just category -> do
            let author = turnAuthor rauthor user
            let postContent = turnContent rcontent author category (maybeToList mtag)
            return Post {..}

evalAuthor :: Select.Author -> Author
evalAuthor  (author :. user) = turnAuthor author user
    --uncurry turnAuthor
--(Row.Author :. Row.User)

evalAuthors :: [Select.Author] -> [Author]
evalAuthors = map evalAuthor

evalComments :: [Select.Comment] -> [Comment]
evalComments = map evalComment

evalComment :: Select.Comment -> Comment
evalComment (comment :. _ :. user) = turnComment comment user


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

----------TURN--------------
--turn from 'row' types to 'json' types
turnContent ::  Row.Content -> Author -> Category -> [Tag]-> Content
turnContent (Row.Content a _ c d _ f g) author category tags  = Content a author c d category f g tags

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author a _ c) user = Author a user c 

turnComment :: Row.Comment -> User -> Comment
turnComment (Row.Comment a _ _ d e) user = Comment a user d e

---------GET-------------------
getPostTags :: Post -> [Tag]
getPostTags = contentTags . postContent

---------SET-------------------
setPostTags :: Post -> [Tag] -> Post
setPostTags post tags = post {postContent = newContent} where
    content = postContent post;
    newContent = content {contentTags = tags}

-------Data manipaltion-------------------
--здесь используется тип Category, который уже проверен на цикличность и корректность в evalCategory
--возможно чистый код заменить везде на код с обработкой ошибочных паттернов!!!

--тут можно упростить, если использовать map вместо списка
evalParams :: ParamsMap Param -> [Category] -> ParamsMap Param
evalParams params categories = res where
    res = M.adjust (\oldParam  -> getChildCategories oldParam categories) "category" params
    -- categoryParam = params ! "category" ;
    -- newCategoryParam = getChildCategories categoryParam categories;
    -- --res = map (\(name, param) -> if name == "category" then (name, newCategoryParam) else (name,param) ) params
    -- res = M.insert "category" newCategoryParam 


getChildCategories :: Param -> [Category] -> Param
getChildCategories (ParamIn vals) cs  = if length filtered == length cs then ParamNo else ParamIn . map (Int . getId) $ filtered where
    cids = map (\(Int cid) -> cid) vals
    filtered = filter (helper cids) cs
    helper :: [Int] -> Category ->  Bool
    helper cids c  = if (getId c) `elem` cids then True else 
        case parent c of
            Nothing -> False 
            Just p -> helper cids p
getChildCategories ParamNo cs = ParamNo
getChildCategories param cs = error $ template "Некоректный параметр {0}" [show param]
    