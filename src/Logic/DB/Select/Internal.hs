module Logic.DB.Select.Internal where

import Common.Functions ((<$$>))
import Common.Template (Template (template))
import Data.Map ((!))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query)
import Interface.Class (MCache, MError)
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Templates
  ( brackets,
    concatWithOR,
    exists,
    inSubQueryM,
    toQuery,
    whereAll,
    whereAllM,
    (<+>),
    (<<+>>),
  )
import qualified Interface.MError.Exports as Error
import Logic.DB.Select.Templates (paramToCondition)

-----------------------------Migration-----------------------------------------
migrationsQuery :: Query
migrationsQuery = [sql|SELECT * FROM migrations|]

-----------------------------User----------------------------------------------
usersQuery :: Query
usersQuery = [sql|SELECT * FROM users|]

userQuery :: Int -> Query
userQuery userId = usersQuery <+> template [sql|WHERE users.id = {0}|] [toQuery userId]

filteredUsersQuery :: (MError m, MCache m) => m Query
filteredUsersQuery = return usersQuery <<+>> pagination

-----------------------------Author--------------------------------------------
authorsQuery :: Query
authorsQuery =
  [sql|SELECT * FROM authors
    LEFT JOIN users
    ON authors.user_id = users.id|]

authorQuery :: Int -> Query
authorQuery authorId = authorsQuery <+> template [sql|WHERE authors.id = {0}|] [toQuery authorId]

filteredAuthorsQuery :: (MError m, MCache m) => m Query
filteredAuthorsQuery = return authorsQuery <<+>> pagination

-----------------------------Category------------------------------------------
categoriesQuery :: Query
categoriesQuery = [sql|SELECT * FROM categories|]

categoryQuery :: Int -> Query
categoryQuery categoryId = categoriesQuery <+> template [sql|WHERE categories.id = {0}|] [toQuery categoryId]

filteredCategoriesQuery :: (MError m, MCache m) => m Query
filteredCategoriesQuery = return categoriesQuery <<+>> pagination

-----------------------------Content-------------------------------------------
contentsQuery :: Query
contentsQuery =
  [sql|
    SELECT * FROM contents
    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id|]

-----------------------------Draft---------------------------------------------
draftQuery :: (MError m, MCache m) => Int -> m Query
draftQuery contentId = do
  paramUserId <- authUserIdParam
  let conditions =
        [ return [sql|contents.is_draft = TRUE|],
          return $ template [sql|contents.id = {0}|] [toQuery contentId],
          paramToCondition [sql|users.id|] paramUserId
        ]
  contentsQuery `whereAllM` conditions

filteredDraftsQuery :: (MError m, MCache m) => m Query
filteredDraftsQuery = do
  paramUserId <- authUserIdParam
  let conditions =
        [ return [sql|contents.is_draft = TRUE|],
          paramToCondition [sql|users.id|] paramUserId
        ]
  contentsQuery `whereAllM` conditions <<+>> pagination

-----------------------------Post----------------------------------------------
postQuery :: Int -> Query
postQuery contentId =
  let conditions =
        [ [sql|contents.is_draft = FALSE|],
          template [sql|contents.id = {0}|] [toQuery contentId]
        ]
   in contentsQuery `whereAll` conditions

filteredPostsQuery :: (MError m, MCache m) => m Query
filteredPostsQuery = do
  params <- Cache.getParams
  let conditions =
        [ return [sql|contents.is_draft = FALSE|],
          postIdsSubQuery [sql|tags_to_contents.tag_id|] (params ! "tag_id"),
          containsCondition (params ! "contains"),
          paramToCondition [sql|contents.category_id|] $ params ! "category_id",
          paramToCondition [sql|contents.creation_date|] $ params ! "created_at",
          paramToCondition [sql|contents.content_name|] $ params ! "content_name",
          paramToCondition [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ params ! "author_name",
          paramToCondition [sql|contents.content_text|] $ params ! "content_text"
        ]
  contentsQuery `whereAllM` conditions <<+>> orderBy (params ! "order_by") <<+>> pagination
  where
    postIdsSubQuery :: MError m => Query -> Cache.Param -> m Query
    postIdsSubQuery field (Cache.ParamAll vals) =
      [sql|contents.id|]
        `inSubQueryM` ([sql|SELECT contents.id FROM contents|] `whereAllM` map (existTagSubQuery field . Cache.ParamEq) vals)
    postIdsSubQuery _ Cache.ParamNo = return [sql|TRUE|]
    postIdsSubQuery field param =
      [sql|contents.id|]
        `inSubQueryM` ([sql|SELECT contents.id FROM contents|] `whereAllM` [existTagSubQuery field param])

    containsCondition :: MError m => Cache.Param -> m Query
    containsCondition param = brackets . concatWithOR <$> subConditions
      where
        subConditions =
          sequenceA
            [ paramToCondition [sql|contents.content_name|] param,
              paramToCondition [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
              paramToCondition [sql|contents.content_name|] param,
              paramToCondition [sql|categories.category_name|] param,
              postIdsSubQuery [sql|tags.tag_name|] param
            ]

    -- Search by tag name and id.
    -- We take ids of posts in which at least one of the tags matches the condition.
    -- Then we return all the posts tags with those ids.
    existTagSubQuery :: MError m => Query -> Cache.Param -> m Query
    existTagSubQuery field param = do
      condition <- paramToCondition field param
      return $
        exists $
          [sql| SELECT 1 FROM contents AS contents_with_tag
            LEFT JOIN tags_to_contents ON contents_with_tag.id = tags_to_contents.content_id
            LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
            WHERE contents_with_tag.id = contents.id
            AND |]
            <+> condition

    orderBy :: MError m => Cache.Param -> m Query
    orderBy (Cache.ParamEq (Cache.StringParam paramName)) = return . template [sql|ORDER BY {0}|] <$$> [field]
      where
        field = case paramName of
          "created_at" -> return [sql|contents.creation_date|]
          "author_name" -> return [sql|CONCAT_WS(' ', users.last_name, users.first_name)|]
          "category_id" -> return [sql|contents.category_id|]
          "photos" -> return [sql|array_length(contents.photos, 1)|]
          _ -> Error.throw $ Error.DevError $ template "Wrong parameter {0} in function orderBy" [paramName]
    orderBy Cache.ParamNo = return [sql||]
    orderBy param = Error.throw $ Error.patError "Select.postsQuery (orderBy)" param

-----------------------------Tag-----------------------------------------------
tagsQuery :: Query
tagsQuery = [sql|SELECT * FROM tags|]

tagQuery :: Int -> Query
tagQuery tagId = tagsQuery <+> template [sql|WHERE tags.id = {0}|] [toQuery tagId]

filteredTagsQuery :: (MError m, MCache m) => m Query
filteredTagsQuery = return tagsQuery <<+>> pagination

-----------------------------Comment-------------------------------------------
commentsQuery :: Query
commentsQuery =
  [sql|
    SELECT * FROM comments
      LEFT JOIN posts ON posts.id = comments.post_id
      LEFT JOIN users ON users.id = comments.user_id|]

filteredCommentsQuery :: (MError m, MCache m) => Int -> m Query
filteredCommentsQuery postId = do
  let conditions = [template [sql|posts.id = {0}|] [toQuery postId]]
  commentsQuery `whereAllM` (return <$> conditions) <<+>> pagination

-----------------------------Pagination----------------------------------------
pagination :: (MError m, MCache m) => m Query
pagination = do
  param <- Cache.getParam "page"
  case param of
    Cache.ParamEq (Cache.IntParam page) ->
      return $
        template
          [sql|LIMIT {0} OFFSET {1}|]
          [toQuery quantity, toQuery $ (page -1) * quantity]
      where
        quantity = 20
    _ -> Error.throw $ Error.patError "Select.pagination" param

-----------------------------Auth----------------------------------------------

-- * Admin can READ all publications

authUserIdParam :: (MError m, MCache m) => m Cache.Param
authUserIdParam = do
  auth <- Cache.getAuth
  case auth of
    Cache.Admin _ -> return Cache.ParamNo
    Cache.Authorized userId -> return $ Cache.ParamEq (Cache.IntParam userId)
    _ -> Error.throw Error.authErrorDefault