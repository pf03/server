module Logic.DB.Insert where

import Common.Functions (templateM, (<<$>>))
import Common.Types (Action (..), BSName)
import Control.Monad.Identity (unless, when)
import Data.Map ((!))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Only (..), Query)
import Interface.Class (MCache, MDB, MError, MTrans)
import qualified Interface.MCache.Exports as Cache
import Interface.MCache.Types
  ( APIType
      ( Author,
        Category,
        Comment,
        Content,
        User
      ),
    Auth (AuthAdmin, AuthUser),
    Param (ParamAll, ParamEq, ParamIn, ParamNo, ParamNull),
    ParamsMap,
    QueryType (Insert),
    Val (Date, Int, Str, valInt),
  )
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates (concatWith, list, toQuery, whereAll)
import qualified Interface.MError.Exports as Error
import Logic.DB.Select.Templates (paramToCondition, paramToQuery, valListToQuery, valToQuery)

----------------------------------Migration------------------------------------

insertMigration :: MDB m => String -> m ()
insertMigration name = do
  DB.execute_ [sql|INSERT into migrations (migration_name) values ('{0}')|] [toQuery name]

----------------------------------User-----------------------------------------
insertUser :: MTrans m => m ()
insertUser = do
  params <- Cache.getParams
  checkNotExist "user" "user_login" [sql|SELECT 1 FROM users WHERE users.user_login = {0}|]
  passQuery <- templateM [sql|md5 (CONCAT_WS(' ', {0}, {1}))|] [cell (params ! "user_login"), cell (params ! "pass")]
  DB.insertM
    User
    [sql|INSERT into users (last_name, first_name, avatar, user_login, pass, creation_date, is_admin) values {0}|]
    [ rowEither
        params
        [ Left "last_name",
          Left "first_name",
          Left "avatar",
          Left "user_login",
          Right passQuery,
          Right [sql|current_date|],
          Right [sql|False|]
        ]
    ]

----------------------------------Author---------------------------------------
insertAuthor :: MTrans m => m ()
insertAuthor = do
  params <- Cache.getParams
  checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  checkNotExist "author" "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
  DB.insertM
    Author
    [sql|INSERT into authors (user_id, description)  values {0}|]
    [row params ["user_id", "description"]]

----------------------------------Category-------------------------------------

-- * You cannot insert a category with a non-existent parent,

-- but you can insert a category without a parent, "parent_id" is an optional parameter
insertCategory :: MTrans m => m ()
insertCategory = do
  params <- Cache.getParams
  checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  DB.insertM
    Category
    [sql|INSERT into categories (parent_id, category_name) values {0}|]
    [row params ["parent_id", "category_name"]]

----------------------------------Tag------------------------------------------
insertTag :: MTrans m => m ()
insertTag = do
  params <- Cache.getParams
  checkNotExist "tag" "tag_name" [sql|SELECT 1 FROM tags WHERE tag_name = {0}|]
  DB.insertM
    Cache.Tag
    [sql|INSERT into tags (tag_name)  values ({0})|]
    [paramToQuery $ params ! "tag_name"]

-- Check and execute parts for right sequence
insertTagToContent :: MTrans m => Action -> m ()
insertTagToContent Check = do
  params <- Cache.getParams
  let tagIds = valInt <$> (\(Cache.ParamAll vs) -> vs) (params ! "tag_id") -- :: [Val]
  condition <- paramToCondition [sql|id|] $ ParamIn (Int <$> tagIds)
  checkExistAll "tag_id" tagIds $ [sql|SELECT id FROM tags|] `whereAll` [condition]
insertTagToContent Execute = do
  params <- Cache.getParams
  unless (emptyParam $ params ! "tag_id") $ do
    DB.executeM_
      [sql|INSERT into tags_to_contents (tag_id, content_id) values {0}|]
      [rows params ["tag_id", "content_id"]]

----------------------------------Draft----------------------------------------
insertDraft :: MTrans m => m ()
insertDraft = do
  params <- addAuthAuthorIdParam
  when (params ! "author_id" == ParamEq (Int 1)) $
    Error.throw $
      Error.DBError
        "Unable to create draft from deleted author with id = 1"
  checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
  insertTagToContent Check
  [Only contentId] <-
    DB.queryM
      [sql|INSERT into contents (author_id, content_name, creation_date, category_id, content_text, main_photo, photos, is_draft, post_id) values {0} RETURNING id|]
      [ rowEither
          params
          [ Left "author_id",
            Left "content_name",
            Right [sql|current_date|],
            Left "category_id",
            Left "content_text",
            Left "main_photo",
            Left "photos",
            Right [sql|TRUE|],
            Right [sql|null|]
          ]
      ]
  Cache.addChanged Insert Content 1
  _ <- Cache.addIdParam "content_id" contentId
  insertTagToContent Execute

----------------------------------Post-----------------------------------------
publish :: MTrans m => Int -> m ()
publish draftId = do
  Cache.addIdParam_ "draft_id" draftId
  checkExist "draft_id" [sql|SELECT 1 FROM contents WHERE id = {0} AND is_draft = TRUE|]
  [Only mPostId] <- DB.query [sql|SELECT post_id FROM contents WHERE id = {0}|] [toQuery draftId]
  case mPostId :: Maybe Int of
    Nothing ->
      DB.update
        Content
        [sql|UPDATE contents SET post_id = NULL, is_draft = FALSE WHERE id = {0}|]
        [toQuery draftId] -- first publish
    Just postId -> do
      DB.execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [toQuery postId]
      DB.update
        Content
        [sql|UPDATE contents SET
          (content_name, creation_date, category_id, content_text, main_photo, photos) =
              (SELECT content_name, creation_date, category_id, content_text, main_photo, photos  FROM contents WHERE id = {1})
          WHERE id = {0}|]
        [toQuery postId, toQuery draftId] -- turn draft to post with old postId
      DB.execute_ [sql|UPDATE tags_to_contents SET content_id = {0} WHERE content_id = {1}|] [toQuery postId, toQuery draftId]
      DB.delete Content [sql|DELETE FROM contents WHERE contents.id = {0} |] [toQuery (draftId :: Int)]

----------------------------------Comment--------------------------------------
insertComment :: MTrans m => Int -> m ()
insertComment postId = do
  _ <- addAuthUserIdParam
  params <- Cache.addIdParam "post_id" postId
  when (params ! "user_id" == ParamEq (Int 1)) $
    Error.throw $
      Error.DBError
        "Unable to create comment from deleted author with id = 1"
  checkExist "post_id" [sql|SELECT 1 FROM contents WHERE id = {0} and is_draft = FALSE|]
  checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
  DB.insertM
    Comment
    [sql|INSERT into comments (post_id, user_id, creation_date, comment_text) values {0}|]
    [rowEither params [Left "post_id", Left "user_id", Right [sql|current_date|], Left "comment_text"]]

----------------------------------Common---------------------------------------

-- | Check for entity existence
checkExist :: MTrans m => BSName -> Query -> m ()
checkExist name templ = do
  param <- Cache.getParam name
  case param of
    ParamNo -> return ()
    ParamNull -> return ()
    ParamEq (Int paramId) -> do
      exist <- DB.query templ [toQuery paramId]
      case exist :: [Only Int] of
        [] -> Error.throwDB "Entity {0} = {1} is not exist" [show name, show paramId]
        _ -> return ()
    _ -> Error.throw $ Error.patError "Insert.checkExist" param

-- | Check for all entities existence
checkExistAll :: MTrans m => BSName -> [Int] -> Query -> m ()
checkExistAll name ids templ = do
  exist <- fromOnly <<$>> DB.query_ templ
  when (length exist /= length ids) $ do
    let notExist = filter (`notElem` exist) ids
    Error.throwDB "Entities \"{0}\" from list {1} are not exist" [show name, show notExist]

checkNotExist :: MTrans m => String -> BSName -> Query -> m ()
checkNotExist description name templ = do
  param <- Cache.getParam name
  case param of
    ParamNo -> return ()
    ParamEq v -> do
      exist <- DB.query templ [valToQuery v]
      case exist :: [Only Int] of
        [] -> return ()
        _ -> Error.throwDB "Entity \"{2}\" with {0} = {1} is already exist" [show name, toString v, description]
    _ -> Error.throw $ Error.patError "Insert.checkNotExist" param
  where
    toString :: Val -> String
    toString (Int n) = show n
    toString (Str str) = show str
    toString (Date date) = show date

row :: MError m => ParamsMap -> [BSName] -> m Query
row params names = list <$> mapM (\name -> cell (params ! name)) names

-- * The number of lines is determined by the first parameter, which must be ParamAll

rows :: MError m => ParamsMap -> [BSName] -> m Query
rows params names = result
  where
    (ParamAll first) = params ! head names
    len = length first
    listOfRows = mapM helper [0 .. len -1]
    helper :: MError m => Int -> m Query
    helper n = list <$> mapM (\name -> cellByNumber (params ! name) n) names
    result = concatWith [sql|,|] <$> listOfRows

emptyParam :: Param -> Bool
emptyParam ParamNo = True
emptyParam (ParamAll []) = True
emptyParam (ParamIn []) = True
emptyParam _ = False

rowEither :: MError m => ParamsMap -> [Either BSName Query] -> m Query
rowEither params eNameQueries = list <$> mapM helper eNameQueries
  where
    helper :: MError m => Either BSName Query -> m Query
    helper eNameQuery = case eNameQuery of
      Left name -> cell (params ! name)
      Right query -> return query

cell :: MError m => Param -> m Query
cell (ParamEq val) = return $ valToQuery val
cell (ParamAll vals) = return $ valListToQuery vals
cell ParamNo = return [sql|null|]
cell param = Error.throw $ Error.patError "Insert.cell" param

cellByNumber :: MError m => Param -> Int -> m Query
cellByNumber (ParamEq v) _ = return $ valToQuery v
cellByNumber (ParamAll vs) n = return $ valToQuery (vs !! n)
cellByNumber ParamNo _ = return [sql|null|]
cellByNumber param _ = Error.throw $ Error.patError "Insert.cellByNumber" param

-- * Admin can CREATE only his own publications

addAuthAuthorIdParam :: MTrans m => m ParamsMap
addAuthAuthorIdParam = do
  _ <- addAuthUserIdParam
  paramUserId <- Cache.getParam "user_id"
  query <-
    templateM
      [sql|
        SELECT authors.id FROM authors
          LEFT JOIN users
          ON authors.user_id = users.id
          WHERE users.id = {0}
      |]
      [paramToQuery paramUserId]
  mAuthorId <- fromOnly <<$>> listToMaybe <$> DB.query_ query
  case mAuthorId :: (Maybe Int) of
    Nothing -> Error.throw $ Error.AuthError "This feature is only available for authors"
    Just 1 -> Error.throw $ Error.AuthError "Unable to authenticate deleted author"
    Just authorId -> Cache.addIdParam "author_id" authorId

-- * Admin can CREATE only his own publications (comments)

addAuthUserIdParam :: (MError m, MCache m) => m ParamsMap
addAuthUserIdParam = do
  auth <- Cache.getAuth
  case auth of
    AuthAdmin userId -> Cache.addIdParam "user_id" userId
    AuthUser userId -> Cache.addIdParam "user_id" userId
    _ -> Error.throw Error.authErrorDefault