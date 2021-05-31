module Logic.Pure.API where

import Common.Functions (Template (template))
import Common.Types (PathInfo)
import qualified Data.ByteString as B
import Data.Text (Text, unpack)
import Interface.Class (MError)
import Interface.MCache.Types
  ( API (..),
    APIType (Author, Category, Comment, Draft, Id, Image, Photo, Post, Tag, User),
    Auth (..),
    QueryType (Auth, Delete, Insert, Load, Select, SelectById, Update, Upload),
  )
import qualified Interface.MError.Exports as Error
import Text.Read (readEither)

-- * The router only checks the roles, sometimes the id, BUT the router does not use the database

-- (it does not even have access to the connection)
-- The use of the database is within the competence of API functions.
router :: MError m => B.ByteString -> PathInfo -> Auth -> m API
---AUTH---
router _ ["login"] _ = return $ API Auth []
---FILES---
router _ ["photos", "upload"] _ = return $ API Upload [Photo]
router _ ["photos", fileName] _ = return $ API Load [Image $ unpack fileName]
router _ ["photos"] _ = return $ API Select [Photo]
---DB---
---INSERT---
router _ ["users", "create"] _ = return $ API Insert [User]
router _ ["authors", "create"] (AuthAdmin _) = return $ API Insert [Author]
router _ ["categories", "create"] (AuthAdmin _) = return $ API Insert [Category]
router _ ["tags", "create"] (AuthAdmin _) = return $ API Insert [Tag]
router _ ["drafts", "create"] auth = withUser auth $ API Insert [Draft]
router path ["drafts", n, "publish"] (AuthAdmin _) = withInt path n $ \paramId -> API Insert [Draft, Id paramId, Post]
router path ["posts", n, "comments", "create"] auth = withUserE auth $ withInt path n $ \paramId -> API Insert [Post, Id paramId, Comment]
---UPDATE---
router path ["users", n, "edit"] (AuthAdmin _) = withInt path n $ \paramId -> API Update [User, Id paramId]
router _ ["user", "edit"] auth = withAuth auth $ \paramId -> API Update [User, Id paramId]
router path ["authors", n, "edit"] (AuthAdmin _) = withInt path n $ \paramId -> API Update [Author, Id paramId]
router path ["categories", n, "edit"] (AuthAdmin _) = withInt path n $ \paramId -> API Update [Category, Id paramId]
router path ["tags", n, "edit"] (AuthAdmin _) = withInt path n $ \paramId -> API Update [Tag, Id paramId]
router path ["drafts", n, "edit"] auth = withUserE auth $ withInt path n $ \paramId -> API Update [Draft, Id paramId]
router path ["posts", n, "edit"] auth = withUserE auth $ withInt path n $ \paramId -> API Update [Post, Id paramId]
---DELETE---
router path ["users", n, "delete"] (AuthAdmin _) = withInt path n $ \paramId -> API Delete [User, Id paramId]
router path ["authors", n, "delete"] (AuthAdmin _) = withInt path n $ \paramId -> API Delete [Author, Id paramId]
router path ["categories", n, "delete"] (AuthAdmin _) = withInt path n $ \paramId -> API Delete [Category, Id paramId]
router path ["tags", n, "delete"] (AuthAdmin _) = withInt path n $ \paramId -> API Delete [Tag, Id paramId]
router path ["drafts", n, "delete"] auth = withUserE auth $ withInt path n $ \paramId -> API Delete [Draft, Id paramId]
router path ["posts", n, "delete"] auth = withUserE auth $ withInt path n $ \paramId -> API Delete [Post, Id paramId]
router path ["comments", n, "delete"] auth = withUserE auth $ withInt path n $ \paramId -> API Delete [Comment, Id paramId]
---SELECT MANY---
router _ ["users"] (AuthAdmin _) = return $ API Select [User]
router _ ["authors"] (AuthAdmin _) = return $ API Select [Author]
router _ ["categories"] _ = return $ API Select [Category]
router _ ["tags"] _ = return $ API Select [Tag]
router _ ["posts"] _ = return $ API Select [Post]
router _ ["drafts"] _ = return $ API Select [Draft]
router _ ["posts", n, "comments"] _ = withInt "post_id" n $ \paramId -> API Select [Post, Id paramId, Comment]
--SELECT BY ID---
router path ["users", n] (AuthAdmin _) = withInt path n $ \paramId -> API SelectById [User, Id paramId]
router _ ["user"] auth = withAuth auth $ \paramId -> API SelectById [User, Id paramId]
router path ["authors", n] (AuthAdmin _) = withInt path n $ \paramId -> API SelectById [Author, Id paramId]
router path ["categories", n] _ = withInt path n $ \paramId -> API SelectById [Category, Id paramId]
router path ["tags", n] _ = withInt path n $ \paramId -> API SelectById [Tag, Id paramId]
router path ["posts", n] _ = withInt path n $ \paramId -> API SelectById [Post, Id paramId]
router path ["drafts", n] _ = withInt path n $ \paramId -> API SelectById [Draft, Id paramId]
--UNKNOWN---
router path _ _ = Error.throw $ unknownPathError path

ereadInt :: MError m => B.ByteString -> Text -> m Int
ereadInt path text = do
  let str = unpack text
  Error.catchEither (readEither str) $ \_ -> unknownPathError path

withInt :: MError m => B.ByteString -> Text -> (Int -> API) -> m API
withInt path text f = do
  paramId <- ereadInt path text
  return $ f paramId

-- * Error 401 - API function that requires authorization

withUserE :: MError m => Auth -> m API -> m API
withUserE auth mapi = do
  api <- mapi
  case auth of
    AuthNo -> Error.throw Error.authErrorDefault
    _ -> return api

withUser :: MError m => Auth -> API -> m API
withUser AuthNo _ = Error.throw Error.authErrorDefault
withUser _ api = return api

withAuth :: MError m => Auth -> (Int -> API) -> m API
withAuth AuthNo _ = Error.throw Error.authErrorDefault
withAuth (AuthAdmin userId) f = return $ f userId
withAuth (AuthUser userId) f = return $ f userId

-- * Error 400

unknownPathError :: B.ByteString -> Error.Error
unknownPathError rawPathInfo = Error.RequestError $ template "Unknown path: {0}" [show rawPathInfo]