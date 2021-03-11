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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
--some test cases for debug (ручная проверка)

--создать список всех возможных запросов с опциональными парамерами, и применить их. 
--После чего создать другой список запросов, которые отменяют предыдущие
--возможно сделать это в режиме тестирования
--так убьем двух зайцев - и тестирование, и ревью, как для себя так и по тз
--тесты базируются на изначальном состоянии бд
--тестирование не только запроса, но и роутера
--проверка значений на уникальность, например тегов

--можно сначала отправить по одному параметру, а потом все сразу (они все необязательные)
selectPostQuery :: (String, [(PathInfo, Query)])
selectPostQuery = ("selectPost", zip pathInfos queries) where
    pathInfos = repeat ["posts"]
    queries = map (:[]) query <> [query]
    query = [
            ("created_at__bt", Just "(2018-05-21,2030-05-21)"),
            ("tag_id__in", Just "[1,2,3]"),
            --("tag__in", Just "[\"Haskell\",\"Python\"]"), --внутренние строки в кавычках. Наружные опционально (см ereadMap). Это не работает (нет в ТЗ)
            ("category_id__in", Just "[1,2,3]"),  
            ("created_at__lt", Just "1925-03-20"),
            ("name", Just "мгновенье"),
            ("text__like", Just "glasgow"),
            ("author_name", Just "Денис"), --кириллица здесь не работает, но в постмане работает
            ("contains__like", Just "haskell")
        ]

--неверный user_id
insertAuthorQueries :: (String, [(PathInfo, Query)])
insertAuthorQueries = ("insertAuthor", zip pathInfos queries) where
    pathInfos = repeat ["authors", "create"]
    
    queries =  [
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
insertTagCases :: (String, [(PathInfo, Query)])
insertTagCases = ("insertTag", zip pathInfos queries) where
    pathInfos = repeat ["tags", "create"]
    queries = [
            --некорректные запросы
            [],
            [("name", Nothing)],

            --корректный запрос
            [("name", Just "some_tag")],

            --повторный запрос, должна выскочить ошибка
            [("name", Just "some_tag")]
        ]

insertCategoryCases :: (String, [(PathInfo, Query)])
insertCategoryCases = ("insertCategory", zip pathInfos queries) where
    pathInfos = repeat ["categories", "create"]
    queries = [
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

--работает
deleteUserCases :: (String, [(PathInfo, Query)])
deleteUserCases = ("deleteUser", tuples) where
    tuples = [
            (,) ["users"] [],
            (,) ["authors"] [],
            (,) ["posts"] [],
            (,) ["users", "0", "delete"] [],
            (,) ["users", "1", "delete"] [],
            (,) ["users", "2", "delete"] [],
            (,) ["users", "7", "delete"] [],
            (,) ["users"] [],
            (,) ["authors"] [],
            (,) ["posts"] []
        ]

--работает  
deleteAuthorCases :: (String, [(PathInfo, Query)])
deleteAuthorCases = ("deleteAuthor", tuples) where
    tuples = [
            (,) ["authors"] [],
            (,) ["posts"] [],
            (,) ["authors", "0", "delete"] [],
            (,) ["authors", "1", "delete"] [],
            (,) ["authors", "4", "delete"] [],
            (,) ["authors"] [],
            (,) ["posts"] []
        ]
 
 --работает
deletePostCases :: (String, [(PathInfo, Query)])
deletePostCases = ("deletePost", tuples) where
    tuples = [
            (,) ["posts"] [],
            (,) ["posts", "0", "delete"] [],
            (,) ["posts", "1", "delete"] [],
            (,) ["posts"] [],
            (,) ["posts", "2", "delete"] [],
            (,) ["posts"] []
        ]

--работает
--дату нужно брать текущую, а не присланную пользователем!!!
commentsCases :: (String, [(PathInfo, Query)])
commentsCases = ("selectComment", tuples) where
    tuples = [
            (,) ["posts", "2", "comments"] [],
            (,) ["posts", "1", "comments"] [],

            (,) ["posts", "666", "comments", "create"] [
                ("user_id", Just "2"),
                ("creation_date", Just "2018-05-21"),
                ("text", Just "Some new comment to wrong post")
                ],

            (,) ["posts", "1", "comments", "create"] [
                ("user_id", Just "666"),
                ("creation_date", Just "2018-05-21"),
                ("text", Just "Some new comment from wrong user")
                ],

            (,) ["posts", "1", "comments", "create"] [
                ("user_id", Just "1"),
                ("creation_date", Just "2018-05-21"),
                ("text", Just "Some new comment from deleted user")
                ],

            (,) ["posts", "1", "comments", "create"] [
                ("user_id", Just "2"),
                ("creation_date", Just "2018-05-21"),
                ("text", Just "Some new comment from right user")
                ],

            (,) ["users", "3", "delete"] [],        (,) ["posts", "1", "comments"] [],
            (,) ["comments", "2", "delete"] [],     (,) ["posts", "1", "comments"] [],
            (,) ["posts", "1", "delete"] [],        (,) ["posts", "1", "comments"] []
        ]


cases :: [(String, [(PathInfo, Query)])]
cases = [
    --selectPostQuery,
    --deleteAuthorCases
    -- deletePostCases,
    commentsCases
    ]

-- casesById :: [(String, [PathInfo])]
-- casesById = [
--     ("deleteAuthor", deleteAuthorQueries)
--     ]

-- --ВНИМАНИЕ!!! Данная функция для корректного тестирования сбрасывает БД до изначального состояния!
--отслеживать выходной json можно в файле response.json (vscode обновляет автоматически)
testCases :: IO ()
testCases = runT $ do
    Migrations.allForce  --сброс БД!!!
    forM_ cases $ uncurry listOfTestCasesByOne
    Log.colorTextT Color.Blue Log.Debug  "Все запросы завершены..."

listOfTestCases :: String -> [(PathInfo, Query)] -> T()
listOfTestCases name qs = do
    forM_ (zip [1,2..] qs) $ \(n, (pathInfo, query)) -> do
        catchT (do
            Log.colorTextT Color.Blue Log.Debug  $ template "Проверка {1}, тестовый случай {0}: " [show n, name]
            Log.debugT query
            DB.getJSON (convert $ show pathInfo) pathInfo query
            Log.colorTextT Color.Green Log.Debug  "Запрос успешно завершен..."
            ) $ \e -> do
                Log.colorTextT Color.Yellow Log.Debug  "Запрос НЕуспешно завершен..."
                Log.colorTextT Color.Yellow Log.Debug $ show (e::E)

--пошагово
listOfTestCasesByOne :: String -> [(PathInfo, Query)] -> T()
listOfTestCasesByOne name qs = do
    forM_ (zip [1,2..] qs) $ \(n, (pathInfo, query)) -> do
        catchT (do
            Log.colorTextT Color.Blue Log.Debug  $ template "Проверка {1}, тестовый случай {0}: " [show n, name]
            Log.debugT query
            DB.getJSON (convert $ show pathInfo) pathInfo query
            Log.colorTextT Color.Green Log.Debug  "Запрос успешно завершен. Нажмите Enter для следующего теста..."
            readLnT
            ) $ \e -> do
                Log.colorTextT Color.Yellow Log.Debug  "Запрос НЕуспешно завершен."
                Log.colorTextT Color.Yellow Log.Debug $ show (e::E)
                Log.colorTextT Color.Yellow Log.Debug  " Нажмите Enter для следующего теста..."
                readLnT


-- listOfTestCases :: String -> [Query] -> (Query -> T()) -> T()
-- listOfTestCases name qs f = do
--     forM_ (zip [1,2..] qs) $ \(n, query) -> do
--         catchT (do
--             Log.colorTextT Color.Blue Log.Debug  $ template "Проверка {1}, тестовый случай {0}: " [show n, name]
--             Log.debugT query
--             f query
--             Log.colorTextT Color.Green Log.Debug  "Запрос успешно завершен..."
--             ) $ \e -> do
--                 Log.colorTextT Color.Yellow Log.Debug  "Запрос НЕуспешно завершен..."
--                 Log.colorTextT Color.Yellow Log.Debug $ show (e::E)

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