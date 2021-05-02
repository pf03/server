module Logic.DB.Insert
    ( migration
    , user
    , author
    , category
    , tag
    , tagToContent
    , draft
    , publish
    , photos
    , comment
    , checkExist
    , rowEither
    ) where

-- Our modules
import           Common.Misc
import           Interface.Cache                  as Cache hiding (Cache (..))
import           Interface.DB                     as DB
import           Interface.Error                  as Error
import           Logic.DB.Select                  (cond, p, val)
-- Other modules
import           Control.Monad.Identity           (unless, when)
import           Data.Map                         ((!))
import           Data.Maybe                       (listToMaybe)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types as SQL (Only (..), Query)

----------------------------------Migration------------------------------------

migration :: MDB m => String -> m ()
migration name = do
    DB.execute_ [sql|INSERT into migrations (name) values ('{0}')|] [q name]

----------------------------------User-----------------------------------------
user :: MT m => m ()
user = do
    params <- Cache.getParams
    checkNotExist "user" "login" [sql|SELECT 1 FROM users WHERE users.login = {0}|]
    passQuery <- (return . template [sql|md5 (CONCAT_WS(' ', {0}, {1}))|]) <$$> 
        [cell(params ! "login"), cell(params ! "pass")]
    DB.insert User
        [sql|INSERT into users (last_name, first_name, avatar, login, pass, creation_date, is_admin) values {0}|] <$$>
        [rowEither params
            [ Left "last_name"
            , Left "first_name"
            , Left "avatar"
            , Left "login"
            , Right passQuery
            , Right [sql|current_date|]
            , Right [sql|False|]
            ]]
            
----------------------------------Author---------------------------------------
author :: MT m => m ()
author = do
    params <- Cache.getParams
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    checkNotExist "author" "user_id" [sql|SELECT 1 FROM authors WHERE authors.user_id = {0}|]
    insert Author [sql|INSERT into authors (user_id, description)  values {0}|] <$$>
        [row params ["user_id", "description"]]

----------------------------------Category-------------------------------------
-- * You cannot insert a category with a non-existent parent, 
-- but you can insert a category without a parent, "parent_id" is an optional parameter
category :: MT m => m ()
category = do
    params <- Cache.getParams
    checkExist "parent_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    insert Category [sql|INSERT into categories (parent_id, category_name) values {0}|] <$$>
        [row params ["parent_id", "category_name"]]

----------------------------------Tag------------------------------------------
tag :: MT m => m ()
tag = do
    params <- Cache.getParams
    checkNotExist "tag" "name" [sql|SELECT 1 FROM tags WHERE tags.name = {0}|]
    insert Tag [sql|INSERT into tags (name)  values ({0})|] <$$> [p $ params ! "name"]

-- Check and execute parts for right sequence
tagToContent :: MT m => Action -> m ()
tagToContent Check = do
    params <- Cache.getParams
    let tagIds = valInt <$> (\(ParamAll vs) -> vs) (params ! "tag_id") -- :: [Val]
    c <- cond [sql|id|] $ ParamIn (Int <$> tagIds)
    checkExistAll "tag_id" tagIds $ [sql|SELECT id FROM tags|] `whereAll` [c]
tagToContent Execute = do
    params <- Cache.getParams
    unless (emptyParam $ params ! "tag_id") $ do
        DB.execute_ [sql|INSERT into tags_to_contents (tag_id, content_id) values {0}|] <$$>
            [rows params ["tag_id", "content_id"]]

----------------------------------Draft----------------------------------------
draft :: MT m => m ()
draft = do
    params <- addAuthAuthorIdParam
    when (params ! "author_id" == ParamEq (Int 1)) $ Error.throw $ DBError
        "Unable to create draft from deleted author with id = 1"
    checkExist "category_id" [sql|SELECT 1 FROM categories WHERE categories.id = {0}|]
    tagToContent Check
    [Only cid] <- (DB.query . template
        [sql|INSERT into contents (author_id, name, creation_date, category_id, text, photo) values {0} RETURNING id|]) <$$>
        [rowEither params [Left "author_id", Left "name", Right [sql|current_date|], Left "category_id", Left "text", Left "photo"]]
    Cache.addChanged Insert Content 1
    _ <- Cache.addIdParam "content_id" cid
    tagToContent Execute
    photos
    insert Draft [sql|INSERT into drafts (content_id) values ({0})|] <$$>
        [cell $ ParamEq (Int cid)]

----------------------------------Post-----------------------------------------
publish :: MT m => Int -> m ()
publish pid = do
    params <- Cache.addIdParam "draft_id" pid
    checkExist "draft_id" [sql|SELECT 1 FROM drafts WHERE drafts.id = {0}|]
    [(contentId, mpostId)] <- DB.query <$> template
        [sql|SELECT content_id, post_id FROM drafts WHERE drafts.id = {0}|] <$$>
        [p $ params ! "draft_id" ]
    case mpostId :: Maybe Int of
        Nothing -> do
            insert Post [sql|INSERT into posts (content_id) values ({0})|]
                [q (contentId :: Int)] -- first publish
        Just postId -> do
            [Only oldContentId] <- DB.query $ template [sql|SELECT content_id FROM posts WHERE posts.id = {0}|]
                [q postId]
            update Post [sql|UPDATE posts SET content_id = {0} WHERE posts.id = {1}|] [q contentId, q postId]
            -- delete old content, because it's not used anywhere 
            delete Content [sql|DELETE FROM contents WHERE contents.id = {0} |] [q (oldContentId :: Int)]
            execute_ [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q oldContentId]
            delete Photo [sql|DELETE FROM photos WHERE content_id = {0}|] [q oldContentId]
    delete Draft [sql|DELETE FROM drafts WHERE drafts.id = {0}|] <$$> [p $ params ! "draft_id"]

----------------------------------Comment--------------------------------------
comment :: MT m => Int -> m ()
comment postId = do
    _ <- addAuthUserIdParam
    params <- Cache.addIdParam "post_id" postId
    when (params ! "user_id" == ParamEq (Int 1)) $ Error.throw $ DBError
        "Unable to create comment from deleted author with id = 1"
    checkExist "post_id" [sql|SELECT 1 FROM posts WHERE posts.id = {0}|]
    checkExist "user_id" [sql|SELECT 1 FROM users WHERE users.id = {0}|]
    insert Comment [sql|INSERT into comments (post_id, user_id, creation_date, text) values {0}|] <$$>
        [rowEither params [Left "post_id", Left "user_id", Right [sql|current_date|], Left "text"]]

----------------------------------Photo----------------------------------------
photos :: MT m => m ()
photos = do
    params <- Cache.getParams
    unless (emptyParam $ params ! "photos") $ do
        insert Photo [sql|INSERT into photos (photo, content_id) values {0}|] <$$>
            [rows params ["photos", "content_id"]]

----------------------------------Common---------------------------------------
-- | Check for entity existence
checkExist :: MT m => BSName -> Query -> m ()
checkExist name templ = do
    par <- Cache.getParam name
    helper par where
        helper ParamNo = return ()
        helper ParamNull = return ()
        helper (ParamEq (Int pid)) = do
            exist <- DB.query $ template templ [q pid]
            case exist :: [Only Int] of
                [] -> Error.throw $ DBError $ template
                    "Entity {0} = {1} is not exist" [show name, show pid]
                _ -> return ()
        helper par = Error.throw $ patError "Insert.checkExist" par

-- | Check for all entities existence
checkExistAll :: MT m => BSName -> [Int] -> Query -> m ()
checkExistAll name ids templ = do
    exist <- fromOnly <<$>> DB.query templ
    when (length exist /= length ids) $ do
        let notExist = filter (`notElem` exist) ids
        Error.throw $ DBError $ template "Entities \"{0}\" from list {1} are not exist"
            [show name, show notExist]

checkNotExist :: MT m => String -> BSName -> Query -> m()
checkNotExist description name templ = do
    param <- Cache.getParam name
    helper param where
        helper ParamNo = return ()
        helper (ParamEq v) = do
            exist <- DB.query $ template templ [val v]
            case exist :: [Only Int] of
                [] -> return ()
                _ -> Error.throw $ DBError  (template "Entity \"{2}\" with {0} = {1} is already exist"
                    [show name, toString v, description])
        helper par = Error.throw $ patError "Insert.checkNotExist" par
        toString :: Val -> String
        toString (Int n)  = show n
        toString (Str s)  = show s
        toString (Date d) = show d

row :: MError m => ParamsMap -> [BSName] -> m Query
row params names = list <$> mapM (\name -> cell (params ! name)) names

-- * The number of lines is determined by the first parameter, which must be ParamAll
rows :: MError m => ParamsMap -> [BSName] -> m Query
rows params names = res where
    (ParamAll first) = params ! head names
    len = length first
    listOfRows = mapM helper [0..len-1]
    helper :: MError m => Int -> m Query
    helper n = list <$> mapM (\name -> cellByNumber (params ! name) n) names
    res = DB.concat [sql|,|] <$> listOfRows

emptyParam:: Param -> Bool
emptyParam ParamNo       = True
emptyParam (ParamAll []) = True
emptyParam (ParamIn [])  = True
emptyParam _             = False

rowEither :: MError m => ParamsMap -> [Either BSName Query] -> m Query
rowEither params nqs = list <$> mapM helper nqs where
    helper :: MError m => Either BSName Query -> m Query
    helper nq = case nq of
        Left name -> cell (params ! name)
        Right qu   -> return qu

cell :: MError m => Param -> m Query
cell (ParamEq v) = return $ val v
cell ParamNo     = return [sql|null|]
cell par         = Error.throw $ patError "Insert.cell" par

cellByNumber :: MError m => Param -> Int -> m Query
cellByNumber (ParamEq v) _     = return $ val v
cellByNumber (ParamAll vs) n = return $ val (vs !! n)
cellByNumber ParamNo _         = return [sql|null|]
cellByNumber par _ = Error.throw $ patError "Insert.cellByNumber" par

-- * Admin can CREATE only his own publications
addAuthAuthorIdParam :: MT m => m ParamsMap
addAuthAuthorIdParam = do
    _ <-addAuthUserIdParam
    paramUserId <- Cache.getParam "user_id"
    mauthorId <- fromOnly <<$>> listToMaybe <$> (DB.query . template [sql|
    SELECT authors.id FROM authors
        LEFT JOIN users
        ON authors.user_id = users.id
        WHERE users.id = {0}
    |] <$$> [p paramUserId])
    case mauthorId :: (Maybe Int) of
        Nothing       -> Error.throw $ AuthError "This feature is only available for authors"
        Just 1        -> Error.throw $ AuthError "Unable to authenticate deleted author"
        Just authorId -> Cache.addIdParam "author_id" authorId

-- * Admin can CREATE only his own publications (comments)
addAuthUserIdParam :: (MError m, MCache m) => m ParamsMap
addAuthUserIdParam = do
    auth <- Cache.getAuth
    case auth of
        AuthAdmin userId -> Cache.addIdParam "user_id" userId
        AuthUser userId  -> Cache.addIdParam "user_id" userId
        _                -> Error.throw authErrorDefault


