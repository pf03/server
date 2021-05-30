module Logic.DB.Select.Internal where

import Common.Functions (Template (template), (<$$>))
import Data.Map ((!))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL (Query)
import Interface.Class ( MCache, MError ) 
import qualified Interface.MCache.Exports as Cache
import Interface.MDB.Templates
  ( brackets,
    concatWithOR,
    exists,
    inList,
    inSubqueryM,
    toQuery,
    whereAllM,
    (<+>),
    (<<+>>),
  )
import qualified Interface.MError.Exports as Error

-----------------------------Migration-----------------------------------------
selectMigrationsQuery :: Query
selectMigrationsQuery = [sql|SELECT * FROM migrations|]

-----------------------------User----------------------------------------------
selectUsersQuery :: Query
selectUsersQuery = [sql|SELECT * FROM users|]

usersQuery :: (MError m, MCache m) => m Query
usersQuery = return selectUsersQuery <<+>> pagination

-----------------------------Author--------------------------------------------
selectAuthorsQuery :: SQL.Query
selectAuthorsQuery =
  [sql|SELECT * FROM authors
    LEFT JOIN users
    ON authors.user_id = users.id|]

authorsQuery :: (MError m, MCache m) => m Query
authorsQuery = return selectAuthorsQuery <<+>> pagination

-----------------------------Category------------------------------------------
selectCategoriesQuery :: Query
selectCategoriesQuery = [sql|SELECT * FROM categories|]

categoriesQuery :: (MError m, MCache m) => m Query
categoriesQuery = return selectCategoriesQuery <<+>> pagination

-----------------------------Draft---------------------------------------------
selectDraftsQuery :: Query
selectDraftsQuery =
  [sql|
    SELECT * FROM drafts
    LEFT JOIN contents ON contents.id = drafts.content_id
    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
    LEFT JOIN photos ON photos.content_id = contents.id|]

-----------------------------Post----------------------------------------------
selectPostsQuery :: Query
selectPostsQuery =
  [sql|
    SELECT * FROM posts
    LEFT JOIN contents ON contents.id = posts.content_id
    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
    LEFT JOIN photos ON photos.content_id = contents.id|]

postsQuery :: (MError m, MCache m) => m SQL.Query
postsQuery = do
  params <- Cache.getParams
  let conditions =
        [ postIdsSubquery [sql|tags_to_contents.tag_id|] (params ! "tag_id"),
          containsCondition (params ! "contains"),
          paramToCondition [sql|contents.category_id|] $ params ! "category_id",
          paramToCondition [sql|contents.creation_date|] $ params ! "created_at",
          paramToCondition [sql|contents.name|] $ params ! "name",
          paramToCondition [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ params ! "author_name",
          paramToCondition [sql|contents.text|] $ params ! "text"
        ]
  selectPostsQuery `whereAllM` conditions <<+>> orderBy (params ! "order_by") <<+>> pagination
  where
    postIdsSubquery :: MError m => Query -> Cache.Param -> m Query
    postIdsSubquery field (Cache.ParamAll vals) =
      [sql|posts.id|]
        `inSubqueryM` ([sql|SELECT posts.id FROM posts|] `whereAllM` map (existTagSubquery field . Cache.ParamEq) vals)
    postIdsSubquery _ Cache.ParamNo = return [sql|TRUE|]
    postIdsSubquery field param =
      [sql|posts.id|]
        `inSubqueryM` ([sql|SELECT posts.id FROM posts|] `whereAllM` [existTagSubquery field param])

    containsCondition :: MError m => Cache.Param -> m Query
    containsCondition param = brackets . concatWithOR <$> subConditions
      where
        subConditions =
          sequenceA
            [ paramToCondition [sql|contents.name|] param,
              paramToCondition [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
              paramToCondition [sql|contents.name|] param,
              paramToCondition [sql|categories.category_name|] param,
              postIdsSubquery [sql|tags.name|] param
            ]

    -- Search by tag name and id.
    -- We take ids of posts in which at least one of the tags matches the condition.
    -- Then we return all the posts tags with those ids.
    existTagSubquery :: MError m => Query -> Cache.Param -> m Query
    existTagSubquery field param = do
      condition <- paramToCondition field param
      return $
        exists $
          [sql| SELECT 1 FROM contents
            LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
            LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
            WHERE contents.id = posts.content_id
            AND |]
            <+> condition

    orderBy :: MError m => Cache.Param -> m SQL.Query
    orderBy (Cache.ParamEq (Cache.Str paramName)) = return . template [sql|ORDER BY {0}|] <$$> [field]
      where
        field = case paramName of
          "created_at" -> return [sql|contents.creation_date|]
          "author_name" -> return [sql|CONCAT_WS(' ', users.last_name, users.first_name)|]
          "category_id" -> return [sql|contents.category_id|]
          "photos" ->
            return $
              brackets
                [sql|SELECT COUNT(*) FROM photos WHERE
                  photos.content_id = contents.id|]
          _ -> Error.throw $ Error.DevError $ template "Wrong parameter {0} in function orderBy" [paramName]
    orderBy Cache.ParamNo = return [sql||]
    orderBy param = Error.throw $ Error.patError "Select.postsQuery (orderBy)" param

-----------------------------Tag-----------------------------------------------
selectTagsQuery :: Query
selectTagsQuery = [sql|SELECT * FROM tags|]

tagsQuery :: (MError m, MCache m) => m Query
tagsQuery = return selectTagsQuery <<+>> pagination

-----------------------------Comment-------------------------------------------
selectCommentsQuery :: Query
selectCommentsQuery =
  [sql|
    SELECT * FROM comments
      LEFT JOIN posts ON posts.id = comments.post_id
      LEFT JOIN users ON users.id = comments.user_id
    |]

commentsQuery :: (MError m, MCache m) => Int -> m Query
commentsQuery postId = selectCommentsQuery `whereAllM` (return <$> conditions) <<+>> pagination
  where
    conditions :: [SQL.Query]
    conditions = [template [sql|posts.id = {0}|] [toQuery postId]]

-----------------------------Pagination----------------------------------------
pagination :: (MError m, MCache m) => m SQL.Query
pagination = do
  param <- Cache.getParam "page"
  case param of
    Cache.ParamEq (Cache.Int page) ->
      return $
        template
          [sql|LIMIT {0} OFFSET {1}|]
          [toQuery quantity, toQuery $ (page -1) * quantity]
      where
        quantity = 20
    _ -> Error.throw $ Error.patError "Select.pagination" param

-----------------------------Templates-----------------------------------------
paramToQuery :: MError m => Cache.Param -> m Query
paramToQuery (Cache.ParamEq val) = return $ valToQuery val
paramToQuery param = Error.throw $ Error.patError "Select.paramToQuery" param

valToQuery :: Cache.Val -> Query
valToQuery (Cache.Int int) = toQuery int
valToQuery (Cache.Str str) = template [sql|'{0}'|] [toQuery str]
valToQuery (Cache.Date date) = template [sql|'{0}'|] [toQuery date]

paramToCondition :: MError m => Query -> Cache.Param -> m Query
paramToCondition field param = case param of
  Cache.ParamEq val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamIn vals -> return $ field `inList` map valToQuery vals
  Cache.ParamLt val -> return $ template [sql|{0} < {1}|] [field, valToQuery val]
  Cache.ParamGt val -> return $ template [sql|{0} > {1}|] [field, valToQuery val]
  Cache.ParamBt (val1, val2) -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, valToQuery val1, valToQuery val2]
  Cache.ParamLike (Cache.Str str) -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, toQuery str]
  Cache.ParamLike val -> return $ template [sql|{0} = {1}|] [field, valToQuery val]
  Cache.ParamNo -> return [sql|TRUE|]
  Cache.ParamNull -> return $ template [sql|{0} = null|] [field]
  _ -> Error.throw $ Error.patError "Select.paramToCondition" param

-- * Admin can READ all publications

authUserIdParam :: (MError m, MCache m) => m Cache.Param
authUserIdParam = do
  auth <- Cache.getAuth
  case auth of
    Cache.AuthAdmin _ -> return Cache.ParamNo
    Cache.AuthUser userId -> return $ Cache.ParamEq (Cache.Int userId)
    _ -> Error.throw Error.authErrorDefault