import Common.Types (PathInfo)
import qualified Data.ByteString as B
import Data.Either (isRight)
import Interface.MCache.Types
  ( API (..),
    APIType (Author, Draft, Post),
    Auth (..),
    ParamsMap,
    QueryType (Insert, Select, Update),
  )
import qualified Interface.MError.Exports as Error
import Lib (allShouldSatisfy)
import qualified Logic.Pure.API as API
import qualified Logic.Pure.Params.Functions as Params
import Network.HTTP.Types.URI as HTTP (Query, simpleQueryToQuery)
import Test.Hspec (Spec, describe, hspec, it, shouldSatisfy)

-- | Pure functions are tested in the Either Error.Error monad, the simplest implementation of the MError class
main :: IO ()
main = do
  hspec testParseParams
  hspec testRouter

-- Predicates
isRequestError :: Either Error.Error a -> Bool
isRequestError ma = case ma of
  Left (Error.RequestError _) -> True
  _ -> False

isAuthError :: Either Error.Error a -> Bool
isAuthError ma = case ma of
  Left (Error.AuthError _) -> True
  _ -> False

-----------------------------Params.parseParams--------------------------------------------
parseParams :: API -> Query -> Either Error.Error ParamsMap
parseParams = Params.parseParams

testParseParams :: Spec
testParseParams = describe "Logic.parseParams" $ do
  it "throws request error" $ do
    nothingCase `shouldSatisfy` isRequestError
    insertAuthorWrongCases `allShouldSatisfy` isRequestError
    selectPostWrongCases `allShouldSatisfy` isRequestError
    noParamsWrongCase `shouldSatisfy` isRequestError
    withParamWrongCase `shouldSatisfy` isRequestError
  it "returns result" $ do
    selectPostRightCases `allShouldSatisfy` isRight
    noParamsRightCase `shouldSatisfy` isRight
    withParamRightCase `shouldSatisfy` isRight

-- Request error: Required parameter "description" is not specified
nothingCase :: Either Error.Error ParamsMap
nothingCase = parseParams (API Insert [Author]) [("user_id", Just "1"), ("description", Nothing)]

insertAuthorWrongCases :: [Either Error.Error ParamsMap]
insertAuthorWrongCases =
  parseParams (API Insert [Author])
    <$> map
      simpleQueryToQuery
      [ -- Request error: Required parameter "description" not specified
        [],
        -- Request error: Unsupported request parameter: "foo"
        [("user_id", "1"), ("foo", "bar")],
        -- Request error: The list of query parameters must contain no more than one value
        -- from the list ["user_id"], but there are 2: [(Eq,"user_id",Just "1"),(Eq,"user_id",Just "2")]
        [("user_id", "1"), ("user_id", "2"), ("description", "bar")],
        -- Request error: Unsupported request parameter: "user_id__all"
        [("user_id", "1"), ("user_id__all", "[2,3,5]"), ("description", "bar")],
        -- Request error: The query parameter "user_id" must be an integer
        [("user_id", "bar"), ("description", "bar")],
        -- Request error: Unsupported request parameter: "foo"
        [("user_id", "1"), ("description", "1"), ("foo", "bar")]
      ]

selectPostWrongCases :: [Either Error.Error ParamsMap]
selectPostWrongCases =
  parseParams (API Select [Post])
    <$> map
      simpleQueryToQuery
      [ --Request error: The query parameter "created_at" must be a date in the format YYYY-MM-DD ...
        [("created_at", "1")],
        [("created_at", "foo")],
        [("created_at", "2020.12.12")],
        [("created_at", "2020_12_12")],
        [("created_at", "2020-18-12")],
        [("created_at", "[2020-12-12]")],
        [("created_at__lt", "2020-18-12")],
        [("created_at__lt", "(2020-12-12, 2020-12-12)")],
        [("created_at__lt", "(2020-12-12,2020-12-12)")],
        [("created_at__gt", "2020-18-12")],
        [("created_at__gt", "(2020-12-12,2020-12-12)")],
        [("created_at__bt", "2020-12-12")],
        [("created_at__bt", "[2020-12-12,2020-12-13]")],
        --Request error: Unsupported request parameter: "created_at__like" ...
        [("created_at__like", "2020-12-12")],
        [("created_at__in", "[2020-12-12,2020-12-13]")],
        [("created_at__all", "[2020-12-12,2020-12-13]")],
        [("author_name__eq", "foo")],
        [("author_name__gt", "2")],
        -- Request error: The query parameter "category_id" must be an integer ...
        [("category_id", "foo")],
        -- Request error: Unsupported request parameter: "category_id__all" ...
        [("category_id__all", "[2,3]")],
        -- Request error: The query parameter "category_id__in" must be a list of integers in the format [x,y,z] ...
        [("category_id__in", "(2,3)")],
        -- Request error: Unsupported request parameter: "category_id__lt" ...
        [("category_id__lt", "3")],
        [("category_id__gt", "3")],
        [("category_id__bt", "(2,3)")],
        [("tag_id__lt", "2")],
        [("tag_id__bt", "(2,3)")],
        [("text", "foo")],
        [("contains", "Vasya")],
        [("order_by__like", "created_at")],
        [("order_by__lt", "created_at")],
        [("order_by", "tag_id")],
        [("page__bt", "(2,3)")]
      ]

selectPostRightCases :: [Either Error.Error ParamsMap]
selectPostRightCases =
  parseParams (API Select [Post])
    <$> map
      simpleQueryToQuery
      [ [("created_at", "2020-12-18")],
        [("created_at__lt", "2020-12-18")],
        [("created_at__gt", "2020-12-18")],
        [("created_at__bt", "(2020-12-12,2020-12-13)")],
        [("author_name", "foo")],
        [("author_name__like", "foo")],
        [("category_id", "2")],
        [("category_id__in", "[2,3]")],
        [("tag_id", "2")],
        [("tag_id__in", "[2,3]")],
        [("tag_id__all", "[1,2,3002]")],
        [("content_name", "bar")],
        [("content_text__like", "foo")],
        [("contains__like", "Vasya")],
        [("order_by", "created_at")],
        [("order_by", "author_name")],
        [("order_by", "category_id")],
        [("order_by", "photos")],
        [("page", "2")],
        [ ("created_at__bt", "(2020-12-12,2020-12-13)"),
          ("author_name__like", "foo"),
          ("category_id__in", "[2,3]"),
          ("tag_id", "2"),
          ("content_name", "bar"),
          ("content_text__like", "foo"),
          ("contains__like", "Vasya"),
          ("order_by", "created_at"),
          ("page", "2")
        ]
      ]

-- API function without params
noParamsWrongCase :: Either Error.Error ParamsMap
noParamsWrongCase = parseParams (API Insert [Post]) [("draft_id", Just "1")]

noParamsRightCase :: Either Error.Error ParamsMap
noParamsRightCase = parseParams (API Insert [Post]) []

-- API function that requires at least one of the parameters
withParamWrongCase :: Either Error.Error ParamsMap
withParamWrongCase = parseParams (API Update [Draft]) []

withParamRightCase :: Either Error.Error ParamsMap
withParamRightCase = parseParams (API Update [Draft]) [("category_id", Just "1")]

-----------------------------API.router----------------------------------------

testRouter :: Spec
testRouter = describe "API.router" $ do
  it "throws request error" $ do
    (router "path" <$> wrongRouterCases <*> return AuthNo) `allShouldSatisfy` isRequestError
    (router "path" <$> wrongRouterCases <*> return (AuthUser 3)) `allShouldSatisfy` isRequestError
    (router "path" <$> wrongRouterCases <*> return (AuthAdmin 1)) `allShouldSatisfy` isRequestError
    (router "path" <$> forAdminRouterCases <*> return AuthNo) `allShouldSatisfy` isRequestError
    (router "path" <$> forAdminRouterCases <*> return (AuthUser 3)) `allShouldSatisfy` isRequestError
  it "throws auth error" $ do
    (router "path" <$> forUserRouterCases <*> return AuthNo) `allShouldSatisfy` isAuthError
  it "returns result" $ do
    (router "path" <$> forAllRouterCases <*> return AuthNo) `allShouldSatisfy` isRight
    (router "path" <$> forAllRouterCases <*> return (AuthUser 3)) `allShouldSatisfy` isRight
    (router "path" <$> forAllRouterCases <*> return (AuthAdmin 1)) `allShouldSatisfy` isRight
    (router "path" <$> forUserRouterCases <*> return (AuthUser 3)) `allShouldSatisfy` isRight
    (router "path" <$> forUserRouterCases <*> return (AuthAdmin 1)) `allShouldSatisfy` isRight
    (router "path" <$> forAdminRouterCases <*> return (AuthAdmin 1)) `allShouldSatisfy` isRight

router :: B.ByteString -> PathInfo -> Auth -> Either Error.Error API
router = API.router

wrongRouterCases :: [PathInfo]
wrongRouterCases =
  [ ["foo"],
    ["login", "2"],
    ["posts", "foo", "comments", "create"],
    ["drafts", "3", "create"],
    ["categories", "delete"]
  ]
forAllRouterCases :: [PathInfo]
forAllRouterCases =
  [ ["login"],
    ["photos", "upload"],
    ["users", "create"],
    ["categories", "42"],
    ["tags", "42"],
    ["posts", "42"],
    ["drafts", "42"]
  ]

forUserRouterCases :: [PathInfo]
forUserRouterCases =
  [ ["drafts", "create"],
    ["posts", "55", "comments", "create"],
    ["user", "edit"],
    ["drafts", "55", "edit"],
    ["posts", "666", "edit"],
    ["drafts", "42", "delete"],
    ["posts", "256", "delete"],
    ["comments", "256", "delete"],
    ["user"]
  ]

forAdminRouterCases :: [PathInfo]
forAdminRouterCases =
  [ ["authors", "create"],
    ["categories", "create"],
    ["tags", "create"],
    ["drafts", "5", "publish"],
    ["users", "5", "edit"],
    ["authors", "7", "edit"],
    ["categories", "99", "edit"],
    ["tags", "55", "edit"],
    ["users", "55", "delete"],
    ["authors", "654", "delete"],
    ["tags", "42", "delete"],
    ["users"],
    ["authors", "256"]
  ]
