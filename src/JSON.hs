{-# LANGUAGE DeriveGeneric #-}
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
import Control.Monad.Except
import Common
import Data.List
import Control.Monad.Trans.Except
import Database.PostgreSQL.Simple
import Class
import qualified Data.Map as M
import qualified Error
import Error (MError)

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
    contentTags :: [Tag],
    contentPhotos :: [Photo]
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
type Photo = Row.Photo

-- getPosts:: [Select.Post] -> [JSON.Post] 
-- getPosts = undefined

----------EVALUATE--------------
--такой ответ кажется избыточный, но по условиям УЧЕБНОГО проекта требуются именно вложенные сущности.

--evaluate from 'select' types to 'json' types
-- evalCategories :: [Select.Category] -> Except E [Category]
-- evalCategories rcs = mapM (evalCategory [] rcs . Row.categoryId) rcs

--здесь лучше упорядочить функции!!!!!!!!!!!!!!!!

evalCategories :: MError m => [Select.Category] -> [Select.Category] -> m [Category]
evalCategories allCategories  = mapM (_evalCategory [] allCategories . Row.categoryId)

evalCategory :: MError m => [Select.Category] -> Select.Category -> m Category
evalCategory  allCategories category = _evalCategory [] allCategories (Row.categoryId category)

--intenal
_evalCategory :: MError m => [Int] -> [Select.Category] -> Int -> m Category
_evalCategory  childs rcs categoryId = do
    if categoryId `elem` childs then do
        --циклическая категория повесит наш сервак при формировании json, поэтому выкинем ошибку
        Error.throw . DBError $ template "Обнаружена циклическая категория {0}, которая является своим же родителем" [show categoryId]
    else do
        let mrc = findById categoryId rcs --двух категорий с одинаковым первичным ключом быть не может. Но может быть Nothing
        case mrc of 
            Nothing -> Error.throw . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Row.Category categoryId mparentId name) -> do
                case mparentId of
                    Nothing -> return $ Category categoryId Nothing name  
                    Just parentId -> do
                        parentCategory <- _evalCategory (categoryId:childs) rcs parentId 
                        return $ Category categoryId (Just parentCategory) name 

--не учтен вариант корневой категории в params
--категория не должна быть своим же родителем
checkCyclicCategory :: MError m => Int -> ParamsMap Param -> [Select.Category] -> m ()
checkCyclicCategory categoryId params rcs = do
    case params M.! "parent_id" of
        ParamNo -> return () --не меняем parent_id
        ParamNull -> return () --меняем parent_id на null, т. е. делаем корневую категорию
        ParamEq (Int parentId) -> do
            grandParents <- getParents parentId rcs
            when (categoryId `elem` grandParents) $ 
                Error.throw . DBError $ template "Категрия {0} имеет в списке родителей {1} категорию {2}. Невозможно создать циклическую категорию" 
                    [show parentId, show grandParents, show categoryId]
    
    
getParents :: MError m => Int -> [Select.Category] -> m [Int]
getParents = helper [] where
    helper :: MError m => [Int] -> Int -> [Select.Category] -> m [Int]
    helper acc categoryId rcs = do
        let mrc = findById categoryId rcs
        case mrc of 
            Nothing -> Error.throw . DBError $ template "Отсутствует категория {0}" [show categoryId]
            Just (Row.Category categoryId mparentId name) -> case mparentId of
                Nothing -> return acc
                Just parentId -> do
                    when (parentId `elem` acc) $
                        Error.throw . DBError $ template "Категория {0} является своим же родителем" [show parentId]
                    helper ([parentId] <> acc) parentId rcs

getCategoryById :: MError m => Int -> [Category] -> String -> m Category
getCategoryById cid cs err = let mcategory = findById cid cs in 
    case mcategory of
        Nothing -> do
            Error.throw . DBError $ err
        Just category -> return category

evalDrafts :: MError m => [Category] -> [Select.Draft] -> m [Draft]
evalDrafts cs l = uniteDrafts <$> mapM (evalDraft cs) l  

evalPosts :: MError m => [Category] -> [Select.Post] -> m [Post]
evalPosts cs l = unitePosts <$> mapM (evalPost cs) l 


evalPost :: MError m => [Category] -> Select.Post -> m Post
evalPost cs (rpost :. rcontent :. rcategory :.  rauthor :. user :. _ :. mtag :. mphoto) = do
    let author = turnAuthor rauthor user
    let postId = Row.postId rpost
    let categoryId = Row.categoryId rcategory
    category <- getCategoryById categoryId cs $ template "Пост {0} принадлежит к несуществующей категории {1}" [show postId, show categoryId]
    let content = turnContent rcontent author category (maybeToList mtag) (maybeToList mphoto)
    let post = turnPost rpost content
    return post 

evalDraft :: MError m => [Category] -> Select.Draft -> m Draft
evalDraft cs (rdraft :. rcontent :. rcategory :.  rauthor :. user :. _ :. mtag :. mphoto) = do
    let author = turnAuthor rauthor user
    let draftId = Row.draftId rdraft
    let categoryId = Row.categoryId rcategory
    category <- getCategoryById categoryId cs $ template "Черновик {0} принадлежит к несуществующей категории {1}" [show draftId, show categoryId]
    let content = turnContent rcontent author category (maybeToList mtag) (maybeToList mphoto)
    let draft = turnDraft rdraft content
    return draft 

unitePosts :: [Post] -> [Post]
unitePosts = map (modifyPostTags filterById . modifyPostPhotos filterById) . unite appendPost where
    appendPost ::  Post -> Post -> Post
    appendPost p1 p2 = setPostTags tags . setPostPhotos photos $ p1 where
        tags = getPostTags p1 <> getPostTags p2;
        photos = getPostPhotos p1 <> getPostPhotos p2

uniteDrafts :: [Draft] -> [Draft]
uniteDrafts = map (modifyDraftTags filterById . modifyDraftPhotos filterById) . unite appendDraft where
    appendDraft ::  Draft -> Draft -> Draft
    appendDraft p1 p2 = setDraftTags tags . setDraftPhotos photos $ p1 where
        tags = getDraftTags p1 <> getDraftTags p2;
        photos = getDraftPhotos p1 <> getDraftPhotos p2

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
turnContent ::  Row.Content -> Author -> Category -> [Tag] -> [Photo] -> Content
turnContent (Row.Content a _ c d _ f g) author category = Content a author c d category f g --tags photos

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author a _ c) user = Author a user c 

turnComment :: Row.Comment -> User -> Comment
turnComment (Row.Comment a _ _ d e) user = Comment a user d e

turnPost :: Row.Post -> Content -> Post
turnPost (Row.Post a _) = Post a --content

turnDraft :: Row.Draft -> Content -> Draft
turnDraft (Row.Draft a _ c) content = Draft a content c

---------GET-------------------
getPostTags :: Post -> [Tag]
getPostTags = contentTags . postContent

getPostPhotos :: Post -> [Photo]
getPostPhotos = contentPhotos . postContent

getDraftTags :: Draft -> [Tag]
getDraftTags = contentTags . draftContent

getDraftPhotos :: Draft -> [Photo]
getDraftPhotos = contentPhotos . draftContent

---------SET-------------------
setPostTags :: [Tag] -> Post -> Post
setPostTags tags post = post {postContent = newContent} where
    content = postContent post;
    newContent = content {contentTags = tags}

setPostPhotos :: [Photo] -> Post -> Post
setPostPhotos photos post = post {postContent = newContent} where
    content = postContent post;
    newContent = content {contentPhotos = photos}

modifyPostTags ::  ([Tag] -> [Tag]) -> Post -> Post
modifyPostTags f post = setPostTags (f $ getPostTags post) post

modifyPostPhotos ::  ([Photo] -> [Photo]) -> Post -> Post
modifyPostPhotos f post = setPostPhotos  (f $ getPostPhotos post) post

setDraftTags :: [Tag] -> Draft -> Draft
setDraftTags tags draft = draft {draftContent = newContent} where
    content = draftContent draft;
    newContent = content {contentTags = tags}

setDraftPhotos :: [Photo] -> Draft -> Draft
setDraftPhotos photos draft = draft {draftContent = newContent} where
    content = draftContent draft;
    newContent = content {contentPhotos = photos}

modifyDraftTags ::  ([Tag] -> [Tag]) -> Draft -> Draft
modifyDraftTags f draft = setDraftTags (f $ getDraftTags draft) draft

modifyDraftPhotos ::  ([Photo] -> [Photo]) -> Draft -> Draft
modifyDraftPhotos f draft = setDraftPhotos  (f $ getDraftPhotos draft) draft

setContentTags :: Content -> [Tag] -> Content
setContentTags content tags = content {contentTags = tags}

setPostContent :: Post -> Content -> Post
setPostContent post content = post {postContent = content}

-------Data manipaltion-------------------
--здесь используется тип Category, который уже проверен на цикличность и корректность в evalCategory
--возможно чистый код заменить везде на код с обработкой ошибочных паттернов!!!

--тут можно упростить, если использовать map вместо списка
evalParams :: [Category] -> ParamsMap Param-> ParamsMap Param
evalParams categories = M.adjust (`getChildCategories` categories) "category"

getChildCategories :: Param -> [Category] -> Param
getChildCategories (ParamIn vals) cs  = if length filtered == length cs then ParamNo else ParamIn . map (Int . getId) $ filtered where
    cids = map (\(Int cid) -> cid) vals
    filtered = filter (helper cids) cs
    helper :: [Int] -> Category ->  Bool
    helper cids c  = (getId c `elem` cids) ||
        case parent c of
            Nothing -> False 
            Just p -> helper cids p
getChildCategories ParamNo cs = ParamNo
getChildCategories param cs = error $ template "Некоректный параметр {0}" [show param]

--универсальная функция для объединения строк
unite :: (Identifiable a) => (a -> a -> a) -> [a] -> [a]
unite f = foldl helper [] where
    helper acc a = updateInsertById (f a) a acc





    