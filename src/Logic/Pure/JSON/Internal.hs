module Logic.Pure.JSON.Internal where

import Logic.Pure.JSON.Types
    ( Photo,
      Tag,
      User,
      Comment(Comment),
      Category(parent),
      Author(Author),
      Content(Content, contentPhotos, contentTags),
      Draft(Draft, draftContent),
      Post(Post, postContent) )
import Common.Identifiable ( updateInsertById, Identifiable(..) )
import Interface.Class (MError)
import Interface.MCache.Types( Param (ParamIn, ParamNo),Val (Int))
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row

-----------------------------Turn----------------------------------------------
-- Turn from 'Row' types to 'JSON' types

turnContent :: Row.Content -> Author -> Category -> [Tag] -> [Photo] -> Content
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
setPostTags tags post = post {postContent = newContent}
  where
    content = postContent post
    newContent = content {contentTags = tags}

setPostPhotos :: [Photo] -> Post -> Post
setPostPhotos photos post = post {postContent = newContent}
  where
    content = postContent post
    newContent = content {contentPhotos = photos}

modifyPostTags :: ([Tag] -> [Tag]) -> Post -> Post
modifyPostTags f post = setPostTags (f $ getPostTags post) post

modifyPostPhotos :: ([Photo] -> [Photo]) -> Post -> Post
modifyPostPhotos f post = setPostPhotos (f $ getPostPhotos post) post

setDraftTags :: [Tag] -> Draft -> Draft
setDraftTags tags draft = draft {draftContent = newContent}
  where
    content = draftContent draft
    newContent = content {contentTags = tags}

setDraftPhotos :: [Photo] -> Draft -> Draft
setDraftPhotos photos draft = draft {draftContent = newContent}
  where
    content = draftContent draft
    newContent = content {contentPhotos = photos}

modifyDraftTags :: ([Tag] -> [Tag]) -> Draft -> Draft
modifyDraftTags f draft = setDraftTags (f $ getDraftTags draft) draft

modifyDraftPhotos :: ([Photo] -> [Photo]) -> Draft -> Draft
modifyDraftPhotos f draft = setDraftPhotos (f $ getDraftPhotos draft) draft

setContentTags :: Content -> [Tag] -> Content
setContentTags content tags = content {contentTags = tags}

setPostContent :: Post -> Content -> Post
setPostContent post content = post {postContent = content}

-----------------------------Data manipulation----------------------------------
-- Here the JSON.Category type is used, which has already been checked for cyclicity and correctness in JSON.evalCategory

getChildCategories :: MError m => Param -> [Category] -> m Param
getChildCategories (ParamIn vals) categories =
  if length filtered == length categories
    then return ParamNo
    else return . ParamIn . map (Int . getId) $ filtered
  where
    categoryIds = map (\(Int categoryId) -> categoryId) vals
    filtered = filter helper categories
    helper :: Category -> Bool
    helper category = (getId category `elem` categoryIds) || maybe False helper (parent category)
getChildCategories ParamNo _ = return ParamNo
getChildCategories param _ = Error.throw $ Error.patError "JSON.getChildCategories" param

-- | Universal function for concatenating rows
unite :: (Identifiable a) => (a -> a -> a) -> [a] -> [a]
unite f = foldl helper []
  where
    helper acc a = updateInsertById (f a) a acc