module Logic.DB.Select.Internal where

import Common.Functions ( Template(template), (<$$>) )
import Data.Map as M ((!))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL ( Query )
import Interface.Class ( MError, MCache )
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import Interface.MDB.Templates
    ( whereAllM,
      concatWithOR,
      (<+>),
      (<<+>>),
      inList,
      inSubqueryM,
      exists,
      brackets,
      toQuery )
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row

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
  pars <- Cache.getParams
  let par name = pars ! name
  let conditions =
        [ postIdsSubquery [sql|tags_to_contents.tag_id|] (par "tag_id"),
          containsCond (par "contains"),
          cond [sql|contents.category_id|] $ par "category_id",
          cond [sql|contents.creation_date|] $ par "created_at",
          cond [sql|contents.name|] $ par "name",
          cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] $ par "author_name",
          cond [sql|contents.text|] $ par "text"
        ]
  selectPostsQuery `whereAllM` conditions <<+>> orderBy (par "order_by") <<+>> pagination
  where
    postIdsSubquery :: MError m => Query -> Cache.Param -> m Query
    postIdsSubquery field (Cache.ParamAll vals) =
      [sql|posts.id|]
        `inSubqueryM` ([sql|SELECT posts.id FROM posts|] `whereAllM` map (existTagSubquery field . Cache.ParamEq) vals)
    postIdsSubquery _ Cache.ParamNo = return [sql|TRUE|]
    postIdsSubquery field param =
      [sql|posts.id|]
        `inSubqueryM` ([sql|SELECT posts.id FROM posts|] `whereAllM` [existTagSubquery field param])

    containsCond :: MError m => Cache.Param -> m Query
    containsCond param = brackets . concatWithOR <$> subConditions
      where
        subConditions =
          sequenceA
            [ cond [sql|contents.name|] param,
              cond [sql|CONCAT_WS(' ', users.last_name, users.first_name)|] param,
              cond [sql|contents.name|] param,
              cond [sql|categories.category_name|] param,
              postIdsSubquery [sql|tags.name|] param
            ]

    -- Search by tag name and id.
    -- We take ids of posts in which at least one of the tags matches the condition.
    -- Then we return all the posts tags with those ids.
    existTagSubquery :: MError m => Query -> Cache.Param -> m Query
    existTagSubquery field param = do
      c <- cond field param
      return $
        exists $
          [sql| SELECT 1 FROM contents

                    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
                    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
                    WHERE contents.id = posts.content_id
                    AND |]
            <+> c

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
    orderBy par = Error.throw $ Error.patError "Select.postsQuery (orderBy)" par

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
    conditions =
      [ template [sql|posts.id = {0}|] [toQuery postId]
      ]

-----------------------------Pagination----------------------------------------
pagination :: (MError m, MCache m) => m SQL.Query
pagination = do
  paramPage <- Cache.getParam "page"
  case paramPage of
    Cache.ParamEq (Cache.Int page) -> return $ template [sql|LIMIT {0} OFFSET {1}|] [toQuery quantity, toQuery $ (page -1) * quantity]
      where
        quantity = 20
    _ -> Error.throw $ Error.patError "Select.pagination" paramPage

-----------------------------Templates-----------------------------------------
p :: MError m => Cache.Param -> m Query
p (Cache.ParamEq v) = return $ val v
p param = Error.throw $ Error.patError "Select.p" param

val :: Cache.Val -> Query
val (Cache.Int a) = toQuery a
val (Cache.Str a) = template [sql|'{0}'|] [toQuery a]
val (Cache.Date a) = template [sql|'{0}'|] [toQuery a]

cond :: MError m => Query -> Cache.Param -> m Query
cond field param = case param of
  Cache.ParamEq v -> return $ template [sql|{0} = {1}|] [field, val v]
  Cache.ParamIn vs -> return $ field `inList` map val vs
  Cache.ParamLt v -> return $ template [sql|{0} < {1}|] [field, val v]
  Cache.ParamGt v -> return $ template [sql|{0} > {1}|] [field, val v]
  Cache.ParamBt (v1, v2) -> return $ template [sql|{0} BETWEEN {1} AND {2}|] [field, val v1, val v2]
  Cache.ParamLike (Cache.Str s) -> return $ template [sql|{0} ILIKE '%{1}%'|] [field, toQuery s]
  Cache.ParamLike v -> return $ template [sql|{0} = {1}|] [field, val v]
  Cache.ParamNo -> return [sql|TRUE|]
  Cache.ParamNull -> return $ template [sql|{0} = null|] [field]
  _ -> Error.throw $ Error.patError "Insert.pagination" param

-- * Admin can READ all publications

authUserIdParam :: (MError m, MCache m) => m Cache.Param
authUserIdParam = do
  a <- Cache.getAuth
  case a of
    Cache.AuthAdmin _ -> return Cache.ParamNo
    Cache.AuthUser userId -> return $ Cache.ParamEq (Cache.Int userId)
    _ -> Error.throw Error.authErrorDefault