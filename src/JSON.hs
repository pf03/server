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

data Draft = Draft {
    draftId :: Int,
    draftContent :: Content,
    draftPostId :: Maybe Int  --или Maybe draftPost??
} deriving (Show, Generic)
instance ToJSON Draft
instance Identifiable Draft where
    getId = draftId

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

--не учтен вариант корневой категории в params
--категория не должна быть своим же родителем
checkCyclicCategory :: Int -> ParamsMap Param -> [Select.Category] -> Except E ()
checkCyclicCategory categoryId params rcs = do
    case params M.! "parent_id" of
        ParamNo -> return () --не меняем parent_id
        ParamNull -> return () --меняем parent_id на null, т. е. делаем корневую категорию
        ParamEq (Int parentId) -> do
            grandParents <- getParents parentId rcs
            when (categoryId `elem` grandParents) $ 
                throwE . DBError $ template "Категрия {0} имеет в списке родителей {1} категорию {2}. Невозможно создать циклическую категорию" 
                    [show parentId, show grandParents, show categoryId]

--категория не должна быть своим же родителем
-- checkCyclicCategory_ :: Int -> Int -> [Select.Category] -> Except E ()
-- checkCyclicCategory_ categoryId parentId rcs = do
--     grandParents <- getParents parentId rcs
--     when (categoryId `elem` grandParents) $ 
--         throwE . DBError $ template "Категрия {0} имеет в списке родителей {1} категорию {2}. Невозможно создать циклическую категорию" 
--             [show parentId, show grandParents, show categoryId]


-- getParents :: Int -> [Select.Category] -> Except E [Int]
-- getParents categoryId rcs = do
--     let mrc = findById categoryId rcs
--     case mrc of 
--         Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
--         Just (Row.Category categoryId mparentId name) -> do
--             case mparentId of
--                 Nothing -> return []
--                 Just parentId -> do
--                     let parents = parentId <> getParents parentId rcs
--                     if categoryId `elem` parents
--                          throwE . DBError $ template "Обнаружена циклическая ктегория " [show categoryId]


    
    
getParents :: Int -> [Select.Category] -> Except E [Int]
getParents = helper [] where
    helper :: [Int] -> Int -> [Select.Category] -> Except E [Int]
    helper acc categoryId rcs = do
        let mrc = findById categoryId rcs
        case mrc of 
            Nothing -> throwE . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Row.Category categoryId mparentId name) -> do
            case mparentId of
                Nothing -> return acc
                Just parentId -> do
                    when (parentId `elem` acc) $
                        throwE . DBError $ template "Категория {0} является своим же родителем" [show parentId]
                    helper ([parentId] <> acc) parentId rcs


    


-- evalPosts :: [Category] -> [Select.Post] -> Except E [Post]
-- evalPosts cs = mapM (evalPost cs) 

-- evalUnitedPosts::[Category] -> [Select.Post] -> Except E [Post]
-- evalUnitedPosts cs l = unitePosts <$> mapM (evalPost cs) l  

-- --unite equal posts with different tags
-- unitePosts :: [Post] -> [Post]
-- unitePosts = foldr helper [] where
--     helper :: Post -> [Post] -> [Post]
--     helper curPost unitedPosts  = res where 
--         curId = getId curPost;
--         --munitedPost = findById curId unitedPosts;
--         res = if existId curId unitedPosts 
--             then updateById curId (uniteTwoPosts curPost) unitedPosts 
--             else curPost : unitedPosts
--         uniteTwoPosts :: Post -> Post -> Post
--         uniteTwoPosts post1 post2 = setPostTags post1 (getPostTags post1 <> getPostTags post2)

-- -- tag1 = Row.Tag 1 "tag1"
-- -- tag2 = Row.Tag 2 "tag2"

-- evalPost :: [Category] -> Select.Post -> Except E Post
-- evalPost cs (rpost :. rcontent :. _ :.  rauthor :. user :. _ :. mtag)  = do
--     let postId = Row.postId rpost
--     let categoryId = Row.contentCategoryId rcontent
--     let mcategory = findById categoryId cs
--     case mcategory of
--         Nothing -> do
--             throwE . DBError $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
--         Just category -> do
--             let author = turnAuthor rauthor user
--             let postContent = turnContent rcontent author category (maybeToList mtag)
--             return Post {..}

evalUnitedDrafts :: [Category] -> [Select.Draft] -> Except E [Draft]
evalUnitedDrafts = undefined

evalPosts :: [Category] -> [Select.Post] -> [Tag] -> Except E [Post]
evalPosts cs = mapM (evalPost cs) 

evalUnitedPosts :: [Category] -> [Select.Post] -> Except E [Post]
evalUnitedPosts cs l = unitePosts <$> mapM (evalPost cs) l  

getCategoryById :: Int -> [Category] -> Except E Category
getCategoryById cid cs = let mcategory = findById cid cs in 
    case mcategory of
        Nothing -> do
            throwE . DBError $ template "Категория {0} не существует" [show cid]
        Just category -> return category

--unite equal posts with different tags
-- unitePosts :: [Post] -> [Post]
-- unitePosts posts = undefined --map setPostContent . uniteContents . map postContent $ posts


evalPost :: [Category] -> Select.Post -> [Tag] -> Except E Post
evalPost cs (rpost :. rcontent :. rcategory :.  rauthor :. user) tags = do
    let author = turnAuthor rauthor user
    category <- getCategoryById (Row.categoryId rcategory) cs
    let content = turnContent rcontent author category tags
    let post = turnPost rpost content
    return post

evalContents :: [Category] -> [Select.Content] -> Except E [Content]
evalContents cs = mapM (evalContent cs) 

evalUnitedContents::[Category] -> [Select.Content] -> Except E [Content]
evalUnitedContents cs l = uniteContents <$> mapM (evalContent cs) l  

--unite equal contents with different tags
uniteContents :: [Content] -> [Content]
uniteContents = foldr helper [] where
    helper :: Content -> [Content] -> [Content]
    helper curContent unitedContents  = res where 
        curId = getId curContent;
        --munitedPost = findById curId unitedPosts;
        res = if existId curId unitedContents 
            then updateById curId (uniteTwoContents curContent) unitedContents 
            else curContent : unitedContents
        uniteTwoContents :: Content -> Content -> Content
        uniteTwoContents content1 content2 = setContentTags content1 (contentTags content1 <> contentTags content2)

-- tag1 = Row.Tag 1 "tag1"
-- tag2 = Row.Tag 2 "tag2"

evalContent :: [Category] -> Select.Content -> Except E Content
evalContent cs (rcontent :. _ :.  rauthor :. user :. _ :. mtag)  = do
    let cid = Row.contentId rcontent
    let categoryId = Row.contentCategoryId rcontent
    let mcategory = findById categoryId cs
    case mcategory of
        Nothing -> do
            throwE . DBError $ template "Контент {0} принадлежит к несуществующей категории {1}" [show cid, show categoryId]
        Just category -> do
            let author = turnAuthor rauthor user
            return $ turnContent rcontent author category (maybeToList mtag)

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

turnPost :: Row.Post -> Content -> Post
turnPost (Row.Post a _) content = Post a content

---------GET-------------------
getPostTags :: Post -> [Tag]
getPostTags = contentTags . postContent

---------SET-------------------
setPostTags :: Post -> [Tag] -> Post
setPostTags post tags = post {postContent = newContent} where
    content = postContent post;
    newContent = content {contentTags = tags}

setContentTags :: Content -> [Tag] -> Content
setContentTags content tags = content {contentTags = tags}

setPostContent :: Post -> Content -> Post
setPostContent post content = post {postContent = content}

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


--универсальная функция для объединения строк
unite :: (Identifiable a) => (a -> a -> a) -> [a] -> [a]
unite f = foldl helper [] where
    helper acc a = updateSetById (getId a) (f a) a acc

unitePosts :: [Post] -> [Post]
unitePosts = unite unitePost where
     unitePost ::  Post -> Post -> Post
     unitePost post1 post2 = setPostTags post1 $ filterById (getPostTags post1 <> getPostTags post2)


    