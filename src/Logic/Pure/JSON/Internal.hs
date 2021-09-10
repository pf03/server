module Logic.Pure.JSON.Internal where

import Common.Identifiable (Identifiable (getId))
import Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import Interface.Class (MError)
import Interface.MCache.Types (Param (ParamIn, ParamNo), ParamValue (IntParam))
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
turnContent (Row.Content contentId _ name creationDate _ text mainPhoto (PGArray photos) isDraft postId) author category tags =
  Content contentId author name creationDate category text mainPhoto photos tags isDraft postId

turnAuthor :: Row.Author -> User -> Author
turnAuthor (Row.Author authorId _ description) user = Author authorId user description

turnComment :: Row.Comment -> User -> Comment
turnComment (Row.Comment commentId _ _ creationDate text) user = Comment commentId user creationDate text

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
    else return . ParamIn . map (IntParam . getId) $ filtered
  where
    categoryIds = map (\(IntParam categoryId) -> categoryId) vals
    filtered = filter helper categories
    helper :: Category -> Bool
    helper category = (getId category `elem` categoryIds) || maybe False helper (parent category)
getChildCategories ParamNo _ = return ParamNo
getChildCategories param _ = Error.throwServerError $ Error.patError "JSON.getChildCategories" param