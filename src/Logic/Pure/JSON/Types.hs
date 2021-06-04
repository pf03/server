{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Logic.Pure.JSON.Types where
import Common.Identifiable ( Identifiable(..) )
import Common.Types ( Path )
import Data.Aeson ( ToJSON )
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Logic.DB.Row as Row

data Post = Post
  { postId :: Int,
    postContent :: Content
  }
  deriving (Show, Generic)

instance ToJSON Post

instance Identifiable Post where
  getId = postId

data Draft = Draft
  { draftId :: Int,
    draftContent :: Content,
    draftPostId :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Draft

instance Identifiable Draft where
  getId = draftId

data Content = Content
  { contentId :: Int,
    contentAuthor :: Author,
    contentName :: String,
    contentCreationDate :: Row.Date,
    contentCategory :: Category,
    contentText :: Text,
    contentPhoto :: Path,
    contentTags :: [Tag],
    contentPhotos :: [Photo]
  }
  deriving (Show, Generic)

instance ToJSON Content

instance Identifiable Content where
  getId = contentId

data Author = Author
  { authorId :: Int,
    authorUser :: Row.User,
    authorDescription :: String
  }
  deriving (Show, Generic)

instance ToJSON Author

instance Identifiable Author where
  getId = authorId

data Category = Category
  { categoryId :: Int,
    parent :: Maybe Category,
    categoryName :: String
  }
  deriving (Show, Generic)

instance ToJSON Category

instance Identifiable Category where
  getId = categoryId

data Comment = Comment
  { commentId :: Int,
    commentUser :: User,
    commentCreationDate :: Row.Date,
    commentText :: Text
  }
  deriving (Show, Generic)

instance ToJSON Comment

instance Identifiable Comment where
  getId = commentId

type User = Row.User

type Tag = Row.Tag

type Photo = Row.Photo