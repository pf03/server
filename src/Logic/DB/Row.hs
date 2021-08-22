{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.DB.Row where

import Common.Identifiable (Identifiable (..))
import Common.Types (FileName, Path)
import Data.Aeson (ToJSON (toJSON), Value (String))
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import qualified Database.PostgreSQL.Simple.Time as Time
import Database.PostgreSQL.Simple.Types (PGArray)
import GHC.Generics (Generic)

data Migration = Migration
  { migrationId :: Int,
    migrationName :: FileName
  }
  deriving (Show, Generic, FromRow)

instance Identifiable Migration where
  getId = migrationId

data User = User
  { userId :: Int, --snake case for table name
    userFirstName :: String,
    userLastName :: String,
    userAvatar :: String,
    userLogin :: String,
    userPass :: String,
    userCreationDate :: Date,
    userIsAdmin :: Bool
  }
  deriving (Show, Generic, FromRow)

instance ToJSON User

instance Identifiable User where
  getId = userId

data Author = Author
  { authorId :: Int,
    authorUserId :: Int,
    authorDescription :: String
  }
  deriving (Show, Generic, FromRow)

instance ToJSON Author

instance Identifiable Author where
  getId = authorId

data Category = Category
  { categoryId :: Int,
    categoryParent :: Maybe Int,
    categoryName :: String
  }
  deriving (Show, Generic, FromRow)

instance ToJSON Category

instance Identifiable Category where
  getId = categoryId

data Tag = Tag
  { tagId :: Int,
    tagName :: String
  }
  deriving (Show, Generic, FromRow)

instance ToJSON Tag

instance Identifiable Tag where
  getId = tagId

data Content = Content
  { contentId :: Int,
    contentAuthorId :: Int,
    contentName :: String,
    contentCreationDate :: Date,
    contentCategoryId :: Int,
    contentText :: Text,
    contentMainPhoto :: Path,
    contentPhotos :: PGArray Path,
    contentIsDraft :: Bool,
    contentPost :: Maybe Int --the postId this draft is related to (when contentIsDraft = True)
  }
  deriving (Show, Generic, FromRow)

-- instance ToJSON Content

instance Identifiable Content where
  getId = contentId

data TagToContent = TagToContent
  { tagToContentContentId :: Int,
    tagToContentTagId :: Int
  }
  deriving (Show, Generic, FromRow)

instance ToJSON TagToContent

data Comment = Comment
  { commentId :: Int,
    commentPostId :: Int,
    commentUserId :: Int,
    commentCreationDate :: Date,
    commentText :: Text
  }
  deriving (Show, Generic, FromRow)

instance ToJSON Comment

instance Identifiable Comment where
  getId = commentId

instance FromRow (Maybe Tag) where
  fromRow = do
    mTagId <- field
    mTagName <- field
    return $ Tag <$> mTagId <*> mTagName

instance FromRow (Maybe TagToContent) where
  fromRow = do
    mTagToContentContentId <- field
    mTagToContentTagId <- field
    return $ TagToContent <$> mTagToContentContentId <*> mTagToContentTagId

newtype Date = Date Time.Date

instance FromField Date where
  fromField f mbs = Date <$> fromField f mbs

instance Show Date where
  show (Date d) = show d

instance ToJSON Date where
  toJSON = String . pack . show