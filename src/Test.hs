module Test where
import Network.HTTP.Types.URI as HTTP
import qualified DB
import Transformer
import Control.Monad
import qualified Migrations
import qualified Log
import qualified System.Console.ANSI as Color
import Control.Monad.Trans.Except
--import Control.Monad.Catch
--import Control.Monad.Exception
import Common
import Types
--some test cases for debug (ручная проверка)

--создать список всех возможных запросов с опциональными парамерами, и применить их. 
--После чего создать другой список запросов, которые отменяют предыдущие
--возможно сделать это в режиме тестирования
--так убьем двух зайцев - и тестирование, и ревью, как для себя так и по тз
--тесты базируются на изначальном состоянии бд
--тестирование не только запроса, но и роутера
--проверка значений на уникальность, например тегов

--можно сначала отправить по одному параметру, а потом все сразу (они все необязательные)
selectPostQuery :: Query
selectPostQuery = [
        ("created_at__bt", Just "(2018-05-21,2030-05-21)"),
        ("tag_id__in", Just "[1,2,3]"),
        ("tag__in", Just "[\"Haskell\",\"Python\"]"), --внутренние строки в кавычках. Наружные опционально (см ereadMap). Это не работает (нет в ТЗ)
        ("categories__in", Just "[1,2,3]"),  
        ("created_at__lt", Just "1925-03-20"),
        ("name", Just "мгновенье"),
        ("text__like", Just "glasgow"),
        ("author_name", Just "Денис"), --кириллица здесь не работает, но в постмане работает
        ("contains__like", Just "haskell"),
        ("user_id", Just "5"),
        ("description", Just "Unknown author")
    ]

--неверный user_id
insertAuthorQueries :: [Query]
insertAuthorQueries = [
        --некорректные запросы
        [],

        [("user_id", Just "1"),
        ("foo", Just "bar")],

        [("user_id", Just "1"),
        ("description", Nothing)],

        [("user_id", Just "bar"),
        ("description", Just "bar")],

        [("user_id", Just "1"),
        ("description", Just "1"),
        ("foo", Just "bar")],

        --некорректный user_id
        [("user_id", Just "666"),
        ("description", Just "description for user_id=666")],

        --корректный запрос, добавляет автора в таблицу authors
        [("user_id", Just "1"),
        ("description", Just "description for user_id=1")],

        --повторный запрос с тем же user_id не должен пройти
        [("user_id", Just "1"),
        ("description", Just "description2 for user_id=1")]
    ]
insertTagQueries :: [Query]
insertTagQueries = [
        --некорректные запросы
        [],
        [("name", Nothing)],

        --корректный запрос
        [("name", Just "some_tag")],

        --повторный запрос, должна выскочить ошибка
        [("name", Just "some_tag")]
    ]

insertCategoryQueries :: [Query]
insertCategoryQueries = [
        --некорректные запросы
        [],

        --некорректный parent_id
        [("parent_id", Just "666"),
        ("category_name", Just "description for category")],

        --корректный parent_id
        [("parent_id", Just "8"),
        ("category_name", Just "description for category")],

        --корректный повтор
        [("parent_id", Just "8"),
        ("category_name", Just "description for category")],

        --корректная корневая категория
        [("category_name", Just "description2 for category")],

        --некорректная корневая категория (в моей логике Nothing не используется)
        [("parent_id", Nothing),
        ("category_name", Just "description for category")]
    ]

cases :: [(String, [Query], Query -> T())]
cases = [
    --("insertAuthor", insertAuthorQueries, DB.insertAuthor), 
    ("insertCategory", insertCategoryQueries, DB.execute "rowPathInfo" ["categories", "create"] )
    ]

--ВНИМАНИЕ!!! Данная функция для корректного тестирования сбрасывает БД до изначального состояния!
testCases :: IO ()
testCases = runT $ do
    Migrations.allForce  --сброс БД!!!
    forM_ cases $ \(a,b,c) -> listOfTestCases a b c
    Log.colorTextT Color.Blue Log.Debug  "Все запросы завершены..."

listOfTestCases :: String -> [Query] -> (Query -> T()) -> T()
listOfTestCases name qs f = do
    forM_ (zip [1,2..] qs) $ \(n, query) -> do
        catchT (do
            Log.colorTextT Color.Blue Log.Debug  $ template "Проверка {1}, тестовый случай {0}: " [show n, name]
            Log.debugT query
            f query
            Log.colorTextT Color.Green Log.Debug  "Запрос успешно завершен..."
            ) $ \e -> do
                Log.colorTextT Color.Yellow Log.Debug  "Запрос НЕуспешно завершен..."
                Log.colorTextT Color.Yellow Log.Debug $ show (e::E)

--то же самое, только пошагово
-- testCasesByOne :: IO ()
-- testCasesByOne = runT $ do
--     Migrations.allForce  --сброс БД!!!
--     forM_ insertAuthorQueries $ \query -> do
--         catchT (DB.insertAuthor query) $ \e -> do
--             Log.colorTextT Color.Yellow Log.Debug $ show (e::E)

-- testExc :: IO()
-- testExc = showT $ testExcT


-- --он не ловит ошибку, а пробрасывает ее дальше
-- testExcT :: T()
-- testExcT = do 
--     catchT (throwT $ RequestError "foo") (\e -> putStrLnT $ "catch foo"++show e)
--     catchT (throwT $ RequestError "bar") (\e -> putStrLnT $ "catch bar"++show e)