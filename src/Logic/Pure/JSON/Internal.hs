module Logic.Pure.JSON.Internal where

import Common.Identifiable (Identifiable (getId))
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Interface.Class (MError)
import Interface.MCache.Types (Param (ParamIn, ParamNo), Val (Int))
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row
import Logic.Pure.JSON.Types
  ( Author (Author),
    Category (parent),
    Comment (Comment),
    Content (Content, contentTags),
    Tag,
    User,
  )

-----------------------------Turn----------------------------------------------
-- Turn from 'Row' types to 'JSON' types

turnContent :: Row.Content -> Author -> Category -> [Tag] -> Content
turnContent (Row.Content contentId _ contentName contentCreationDate _ contentText contentMainPhoto (PGArray contentPhotos) contentIsDraft contentPostId) author category tags =
  Content contentId author contentName contentCreationDate category contentText contentMainPhoto contentPhotos tags contentIsDraft contentPostId

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author authorId _ authorDescription) user = Author authorId user authorDescription

turnComment :: Row.Comment -> User -> Comment
turnComment (Row.Comment commentId _ _ commentCreationDate commentText) user = Comment commentId user commentCreationDate commentText

-----------------------------Setters-------------------------------------------
setContentTags :: [Tag] -> Content -> Content
setContentTags tags content = content {contentTags = tags}

modifyContentTags :: ([Tag] -> [Tag]) -> Content -> Content
modifyContentTags f content = setContentTags (f $ contentTags content) content

-----------------------------Data manipulation----------------------------------
-- Here the JSON.Category type is used, which has already been checked for cyclic recurrence and correctness in JSON.evalCategory

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