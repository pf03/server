{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.DB.Row where

-- Our Modules
import           Common.Identifiable
import           Common.Misc

-- Other Modules
import           Data.Aeson                         
import           Data.Text                          (Text (..), pack)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Time
import           GHC.Generics

data Migration = Migration {
    migrationId :: Int,
    migrationName :: String,
    migrationDescription :: String
    } deriving (Show, Generic, FromRow)
instance Identifiable Migration where
    getId = migrationId

data User = User {
    userId :: Int, --snake case for table name
    userFirstName :: String,
    userLastName :: String,
    userAvatar :: String,
    userLogin :: String,
    userPass :: String,
    userCreationDate :: Date,
    userIsAdmin :: Bool
} deriving (Show, Generic, FromRow)
instance ToJSON User
instance Identifiable User where
    getId = userId

data Author = Author {
    authorId :: Int,
    authorUserId :: Int,
    authorDescription :: String
} deriving (Show, Generic, FromRow)
instance ToJSON Author
instance Identifiable Author where
    getId = authorId

data Category = Category{
    categoryId :: Int,
    categoryParent :: Maybe Int,
    categoryName :: String
} deriving (Show, Generic, FromRow)
instance ToJSON Category
instance Identifiable Category where
    getId = categoryId

data Tag = Tag {
    tagId :: Int,
    tagName :: String
} deriving (Show, Generic, FromRow)
instance ToJSON Tag
instance Identifiable Tag where
    getId = tagId

data Content = Content {
    contentId :: Int,
    contentAuthorId :: Int,
    contentName :: String,
    contentCreationDate :: Date,
    contentCategoryId :: Int,
    contentText :: Text,
    contentPhoto :: Path
} deriving (Show, Generic, FromRow)
instance ToJSON Content
instance Identifiable Content where
    getId = contentId

data Post = Post {
    postId :: Int,
    postContentId :: Int
} deriving (Show, Generic, FromRow)
instance ToJSON Post
instance Identifiable Post where
    getId = postId

data Draft = Draft {
    draftId :: Int,
    draftContentId :: Int,
    draftPostId :: Maybe Int
} deriving (Show, Generic, FromRow)
instance ToJSON Draft
instance Identifiable Draft where
    getId = draftId

data TagToContent = TagToContent{
    tagToContentId :: Int,
    tagToContentContentId :: Int,
    tagToContentTagId :: Int
} deriving (Show, Generic, FromRow)
instance ToJSON TagToContent
instance Identifiable TagToContent where
    getId = tagToContentId

data Photo = Photo{
    photoId :: Int,
    photoPhoto :: Path,
    photoContentId :: Int
} deriving (Show, Generic, FromRow)
instance ToJSON Photo
instance Identifiable Photo where
    getId = photoId

data Comment = Comment {
    commentId :: Int,
    commentPostId :: Int,
    commentUserId :: Int,
    commentCreationDate :: Date,
    commentText :: Text
} deriving (Show, Generic, FromRow)
instance ToJSON Comment
instance Identifiable Comment where
    getId = commentId



instance FromRow (Maybe Tag) where
    fromRow = do
        mtagId <- field
        mtagName <- field
        return $ Tag <$> mtagId <*> mtagName

instance FromRow (Maybe Photo) where
    fromRow = do
        a <- field
        b <- field
        c <- field
        return $ Photo <$> a <*> b <*> c

instance FromRow (Maybe TagToContent) where
    fromRow = do
        a <- field
        b <- field
        c <- field
        return $ TagToContent <$> a <*> b <*> c

instance ToJSON Date where
  toJSON = String . pack . show