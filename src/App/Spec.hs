module App.Spec where

import Test.Hspec
import Test.QuickCheck
import qualified Logic.Pure.Params as Params
import App.Lib
import           Network.HTTP.Types.URI     as HTTP
import Interface.Cache
import Data.Either
import Interface.Error as Error
import           Common.Misc
import qualified Logic.Pure.API as API
import qualified Data.ByteString as B

-- Сделать рефакторинг
-- ! переместил тесты в папку src для более удобной отладки, потом обратно перемещу в test

--import App.Emulate  --перенести потом тестовые случаи куда-нибудь в отдельный модуль папки test
--и модуль Emulate тоже в test

-- main :: IO ()
-- main = hspec testParseParams

test :: IO ()
test = hspec testParseParams


-----------------------------parseParams--------------------------------------------

parseParams :: API -> Query -> Either E ParamsMap
parseParams = Params.parseParams

router :: B.ByteString -> PathInfo -> Auth -> Either E API
router = API.router

testParseParams :: Spec
testParseParams = do
    describe "Logic.parseParams" $ do 
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
    describe "API.router" $ do 
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
            
isRequestError :: Either E a -> Bool 
isRequestError ma = case ma of 
    Left (RequestError _ ) -> True 
    _ -> False

isAuthError :: Either E a -> Bool 
isAuthError ma = case ma of 
    Left (AuthError _ ) -> True 
    _ -> False

--Ошибка веб-запроса: Не указано значение обязательного параметра "description"
nothingCase :: Either E ParamsMap 
nothingCase = parseParams (API Insert [Author]) [("user_id", Just "1"), ("description", Nothing)]

showTest = showCases (router "path" <$> wrongRouterCases <*> return AuthNo)

showCases :: Show a => [Either E a] -> IO ()
showCases = mapM_ $ \c -> do
    case c of 
        Left e -> print e
        Right a -> putStrLn "right!"
    

insertAuthorWrongCases :: [Either E ParamsMap]
insertAuthorWrongCases = parseParams (API Insert [Author]) <$> map simpleQueryToQuery [
        --Ошибка веб-запроса: Не указан обязательный параметр "description"
        [],
        --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
        [("user_id", "1"), ("foo", "bar")],
        -- Ошибка веб-запроса: В списке параметров запроса должно быть не более одного 
        -- значения из списка ["user_id"], а их 2: [(Eq,"user_id",Just "1"),(Eq,"user_id",Just "2")]
        [("user_id", "1"), ("user_id", "2"), ("description", "bar")],
        -- Ошибка веб-запроса: Недопустимый параметр запроса: "user_id__all"
        [("user_id", "1"), ("user_id__all", "[2,3,5]"), ("description", "bar")],
        --Ошибка веб-запроса: Параметр запроса "user_id" должен быть целым числом
        [("user_id", "bar"), ("description", "bar")],
        --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
        [("user_id", "1"), ("description", "1"), ("foo", "bar")]
    ]

selectPostWrongCases :: [Either E ParamsMap]
selectPostWrongCases = parseParams (API Select [Post]) <$> map simpleQueryToQuery [
        --Ошибка веб-запроса: Параметр запроса "created_at" должен быть датой в формате YYYY-MM-DD ...
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
        --Ошибка веб-запроса: Недопустимый параметр запроса: "created_at__like" ...
        [("created_at__like", "2020-12-12")],
        [("created_at__in", "[2020-12-12,2020-12-13]")],
        [("created_at__all", "[2020-12-12,2020-12-13]")],
        [("author_name__eq", "foo")],
        [("author_name__gt", "2")],
        -- Ошибка веб-запроса: Параметр запроса "category_id" должен быть целым числом ...
        [("category_id", "foo")],
        -- Ошибка веб-запроса: Недопустимый параметр запроса: "category_id__all" ...
        [("category_id__all", "[2,3]")],
        -- Ошибка веб-запроса: Параметр запроса "category_id__in" должен быть массивом, состоящим из целых чисел в формате [x,y,z] ...
        [("category_id__in", "(2,3)")],
        -- Ошибка веб-запроса: Недопустимый параметр запроса: "category_id__lt" ...
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

selectPostRightCases :: [Either E ParamsMap]
selectPostRightCases = parseParams (API Select [Post]) <$> map simpleQueryToQuery [
        [("created_at", "2020-12-18")],
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
        [("name", "bar")],
        [("text__like", "foo")],
        [("contains__like", "Vasya")],
        [("order_by", "created_at")],
        [("order_by", "author_name")],
        [("order_by", "category_id")],
        [("order_by", "photos")],
        [("page", "2")],
        [
            ("created_at__bt", "(2020-12-12,2020-12-13)"), 
            ("author_name__like", "foo"), 
            ("category_id__in", "[2,3]"),
            ("tag_id", "2"),
            ("name", "bar"),
            ("text__like", "foo"),
            ("contains__like", "Vasya"),
            ("order_by", "created_at"),
            ("page", "2") 
            ]
    ]

-- Функция без параметров
noParamsWrongCase :: Either E ParamsMap 
noParamsWrongCase = parseParams (API Insert [Post]) [("draft_id", Just "1")]

noParamsRightCase :: Either E ParamsMap 
noParamsRightCase = parseParams (API Insert [Post]) []

-- Функция, требующая хотя бы один из параметров
withParamWrongCase :: Either E ParamsMap 
withParamWrongCase = parseParams (API Update [Draft]) []

withParamRightCase :: Either E ParamsMap 
withParamRightCase = parseParams (API Update [Draft]) [("category_id", Just "1")]

-----------------------------API.router----------------------------------------

-- ccc :: [Either E API]
-- ccc = API.router "path" <$> wrongRouterCases <*> return AuthNo 

wrongRouterCases :: [PathInfo]
wrongRouterCases = [
    ["foo"],
    ["login", "2"],
    ["posts", "foo", "comments", "create"],
    ["drafts", "3", "create"],
    ["categories", "delete"]
    ]

forAllRouterCases :: [PathInfo]
forAllRouterCases = [
    ["login"],
    ["photos", "upload"],
    ["users", "create"],
    ["categories", "42"],
    ["tags", "42"],
    ["posts", "42"],
    ["drafts", "42"]
    ]

forUserRouterCases :: [PathInfo]
forUserRouterCases = [
    ["drafts", "create"],
    ["posts", "55", "comments", "create"],
    ["user", "edit"],
    ["drafts", "55", "edit"],
    ["posts", "666", "edit"],
    ["drafts", "42", "delete"],
    ["posts", "256", "delete"],
    ["comments", "256", "delete"],
    ["user"]
    ]

forAdminRouterCases ::  [PathInfo]
forAdminRouterCases = [
    ["authors", "create"],
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