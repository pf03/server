module Logic.Pure.JSON.Types where

import Common.Identifiable (Identifiable (..))
import Common.Types (Path)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Logic.DB.Row as Row

data Content = Content
  { contentId :: Int,
    contentAuthor :: Author,
    contentName :: String,
    contentCreationDate :: Row.Date,
    contentCategory :: Category,
    contentText :: Text,
    contentMainPhoto :: Path,
    contentPhotos :: [Path],
    contentTags :: [Tag],
    contentIsDraft :: Bool,
    contentPostId :: Maybe Int --the postId this draft is related to (when contentIsDraft = True)
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