{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.Pure.JSON where

-- Our modules
import           Common.Identifiable
import           Common.Misc
import           Interface.Cache                 as Cache hiding (APIType(..), params)
import           Interface.Error                 as Error 
import qualified Logic.DB.Row                    as Row
import qualified Logic.DB.Select                 as Select

-- Other modules
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Map                        as M
import           Data.Text                       (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time
import           GHC.Generics

data Post = Post {
    postId      :: Int,
    postContent :: Content
} deriving (Show, Generic)
instance ToJSON Post
instance Identifiable Post where
    getId = postId

data Draft = Draft {
    draftId      :: Int,
    draftContent :: Content,
    draftPostId  :: Maybe Int
} deriving (Show, Generic)
instance ToJSON Draft
instance Identifiable Draft where
    getId = draftId

data Content = Content {
    contentId           :: Int,
    contentAuthor       :: Author,
    contentName         :: String,
    contentCreationDate :: Date,
    contentCategory     :: Category,
    contentText         :: Text,
    contentPhoto        :: Path,
    contentTags         :: [Tag],
    contentPhotos       :: [Photo]
} deriving (Show, Generic)
instance ToJSON Content
instance Identifiable Content where
    getId = contentId

data Author = Author {
    authorId          :: Int,
    authorUser        :: Row.User,
    authorDescription :: String
} deriving (Show, Generic)
instance ToJSON Author
instance Identifiable Author where
    getId = authorId

data Category = Category{
    categoryId   :: Int,
    parent       :: Maybe Category,
    categoryName :: String
} deriving (Show, Generic)
instance ToJSON Category
instance Identifiable Category where
    getId = categoryId

data Comment = Comment {
    commentId           :: Int,
    commentUser         :: User,
    commentCreationDate :: Date,
    commentText         :: Text
} deriving (Show, Generic)
instance ToJSON Comment
instance Identifiable Comment where
    getId = commentId

type User = Row.User
type Tag = Row.Tag
type Photo = Row.Photo

-----------------------------Evaluate------------------------------------------
-- Evaluate from 'Select' types to 'JSON' types

-- * This answer seems redundant, but according to the terms of the TRAINING project, required exactly nested entities.
evalCategories :: MError m => [Select.Category] -> [Select.Category] -> m [Category]
evalCategories allCategories = mapM (_evalCategory [] allCategories . Row.categoryId)

evalCategory :: MError m => [Select.Category] -> Select.Category -> m Category
evalCategory  allCategories category = _evalCategory [] allCategories (Row.categoryId category)

-- intenal
_evalCategory :: MError m => [Int] -> [Select.Category] -> Int -> m Category
_evalCategory  childs rcs cid = do
    if cid `elem` childs then do
        -- The cyclic category will froze our server when generating json, so let's throw out the error
        Error.throw . DBError $ 
            template "Cyclic category {0} found, which is its own parent" [show cid]
    else do
        let mrc = findById cid rcs -- There can't be two categories with the same primary key. But it can be no one
        case mrc of
            Nothing -> Error.throw . DBError $ template "Отсутствует категория {0}" [show cid]
            Just (Row.Category _ mparentId name) -> do
                case mparentId of
                    Nothing -> return $ Category cid Nothing name
                    Just parentId -> do
                        parentCategory <- _evalCategory (cid:childs) rcs parentId
                        return $ Category cid (Just parentCategory) name

-- Category should not be its own parent
checkCyclicCategory :: MError m => Int -> ParamsMap -> [Select.Category] -> m ()
checkCyclicCategory cid params rcs = do
    case params M.! "parent_id" of
        ParamNo                 -> return ()
        ParamNull               -> return () -- root category
        ParamEq (Int parentId)  -> do
            grandParents <- getParents parentId rcs
            when (cid `elem` grandParents) $
                Error.throw . DBError $ 
                    template "Category {0} has category {2} in the parent list {1}. Unable to create cyclic category"
                    [show parentId, show grandParents, show cid]
        par                     -> Error.throw $ patError "JSON.checkCyclicCategory" par


getParents :: MError m => Int -> [Select.Category] -> m [Int]
getParents = helper [] where
    helper :: MError m => [Int] -> Int -> [Select.Category] -> m [Int]
    helper acc cid rcs = do
        let mrc = findById cid rcs
        case mrc of
            Nothing -> Error.throw . DBError $ template "Category missing {0}" [show cid]
            Just (Row.Category _ mparentId _) -> case mparentId of
                Nothing -> return acc
                Just parentId -> do
                    when (parentId `elem` acc) $
                        Error.throw . DBError $ template "Category {0} is its own parent" [show parentId]
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
    let pid = Row.postId rpost
    let cid = Row.categoryId rcategory
    category <- getCategoryById cid cs $ 
        template "Post {0} belongs to a category that does not exist {1}" [show pid, show cid]
    let content = turnContent rcontent author category (maybeToList mtag) (maybeToList mphoto)
    let post = turnPost rpost content
    return post

evalDraft :: MError m => [Category] -> Select.Draft -> m Draft
evalDraft cs (rdraft :. rcontent :. rcategory :.  rauthor :. user :. _ :. mtag :. mphoto) = do
    let author = turnAuthor rauthor user
    let did = Row.draftId rdraft
    let cid = Row.categoryId rcategory
    category <- getCategoryById cid cs $ 
        template "Draft {0} belongs to a category that does not exist {1}" [show did, show cid]
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

evalAuthors :: [Select.Author] -> [Author]
evalAuthors = map evalAuthor

evalComments :: [Select.Comment] -> [Comment]
evalComments = map evalComment

evalComment :: Select.Comment -> Comment
evalComment (comment :. _ :. user) = turnComment comment user


maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-----------------------------Turn----------------------------------------------
-- Turn from 'Row' types to 'JSON' types

turnContent ::  Row.Content -> Author -> Category -> [Tag] -> [Photo] -> Content
turnContent (Row.Content a _ c d _ f g) author category = Content a author c d category f g --tags photos

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author a _ c) user = Author a user c

turnComment :: Row.Comment -> User -> Comment
turnComment (Row.Comment a _ _ d e) user = Comment a user d e

turnPost :: Row.Post -> Content -> Post
turnPost (Row.Post a _) = Post a

turnDraft :: Row.Draft -> Content -> Draft
turnDraft (Row.Draft a _ c) content = Draft a content c

-----------------------------Getters-------------------------------------------
getPostTags :: Post -> [Tag]
getPostTags = contentTags . postContent

getPostPhotos :: Post -> [Photo]
getPostPhotos = contentPhotos . postContent

getDraftTags :: Draft -> [Tag]
getDraftTags = contentTags . draftContent

getDraftPhotos :: Draft -> [Photo]
getDraftPhotos = contentPhotos . draftContent

-----------------------------Setters-------------------------------------------
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

-----------------------------Data manipulation----------------------------------
-- Here the JSON.Category type is used, which has already been checked for cyclicity and correctness in JSON.evalCategory

evalParams :: MError m => [Category] -> ParamsMap -> m ParamsMap
evalParams categories = adjustM (`getChildCategories` categories) ParamNo "category_id"

getChildCategories :: MError m => Param -> [Category] -> m Param
getChildCategories (ParamIn vals) cs  = if length filtered == length cs 
        then return ParamNo else return . ParamIn . map (Int . getId) $ filtered where
    cids = map (\(Int cid) -> cid) vals
    filtered = filter helper cs
    helper :: Category ->  Bool
    helper c  = (getId c `elem` cids) || maybe False helper (parent c)
getChildCategories ParamNo _ = return ParamNo
getChildCategories param _ = Error.throw $ patError "JSON.getChildCategories" param

-- | Universal function for concatenating rows
unite :: (Identifiable a) => (a -> a -> a) -> [a] -> [a]
unite f = foldl helper [] where
    helper acc a = updateInsertById (f a) a acc








