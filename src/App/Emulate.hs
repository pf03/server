module App.Emulate where

-- Our Modules
import qualified Logic.DB.Migrations             as Migrations
import           Common.Misc
import           Interface.Error            as Error
import           Interface.Log              as Log
import Interface.DB as DB ( MT )
import           Logic.DB.Auth              as Auth
import qualified Logic.IO.Response          as Response
import           T.Transformer

-- Other Modules
import           Control.Monad
import           Data.Aeson
import           Network.HTTP.Types.URI     as HTTP
import           System.Console.ANSI
import qualified System.Console.ANSI        as Color
import           Text.Read

-- ЭТОТ МОДУЛЬ НЕ ДЛЯ РЕВЬЮ, А ДЛЯ ОТЛАДКИ
-- Эмуляция последовательных запросов к серверу с разной авторизацией

selectPostCases :: (String, [(PathInfo, Query)])
selectPostCases = ("selectPost", zip pathInfos queries) where
    pathInfos = repeat ["posts"]
    queries = map (:[]) query <> [query]
    query = [
            --подобрать более корректные тесты
            ("created_at__bt", Just "(2018-05-21, 2030-05-21)"),
            ("tag_id__in", Just "[1,2,3]"),
            --("tag__in", Just "[\"Haskell\",\"Python\"]"), --внутренние строки в кавычках. Наружные опционально (см ereadMap). Это не работает (нет в ТЗ)
            ("category_id__in", Just "[1,2,3]"),
            ("name", Just "мгновенье"),  --кириллица здесь не работает, но в постмане работает
            ("text__like", Just "glasgow"),
            ("author_name", Just "Денис"), --кириллица здесь не работает, но в постмане работает
            ("contains__like", Just "haskell")
        ]

insertAuthorCases :: (String, [(PathInfo, Query)])
insertAuthorCases = ("insertAuthor", zip pathInfos queries) where
    pathInfos = repeat ["authors", "create"]
    queries =  [
            --некорректные запросы
            --Ошибка веб-запроса: Не указан обязательный параметр "description"
            [],

            --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
            [("user_id", Just "1"),
            ("foo", Just "bar")],

            --Ошибка веб-запроса: Не указано значение параметра "description"
            [("user_id", Just "1"),
            ("description", Nothing)],

            --Ошибка веб-запроса: Параметр запроса "user_id" должен быть целым числом
            [("user_id", Just "bar"),
            ("description", Just "bar")],

            --Ошибка веб-запроса: Недопустимый параметр запроса: "foo"
            [("user_id", Just "1"),
            ("description", Just "1"),
            ("foo", Just "bar")],

            --Ошибка базы данных: Указан несуществующий параметр "user_id": 666
            [("user_id", Just "666"),
            ("description", Just "description for user_id=666")],

            --КОРРЕКТНЫЙ ЗАПРОС
            [("user_id", Just "2"),
            ("description", Just "description for user_id=1")],

            --Ошибка базы данных: Автор с таким "user_id" = 2 уже существует
            [("user_id", Just "2"),
            ("description", Just "description2 for user_id=1")]
        ]

updateAuthorCases :: (String, [(PathInfo, Query)])
updateAuthorCases = ("author", tuples) where
    tuples = [
            (,) ["authors"] [],

            -- Ошибка веб-запроса: Необходимо указать хотя бы один параметр для редактирвания из следующего списка : ["description","user_id"]
            (,) ["authors", "0", "edit"] [],

            -- Ошибка базы данных: Указан несуществующий параметр "id": 0
            (,) ["authors", "0", "edit"] [
                    ("user_id", Just "666"),
                    ("description", Just "foo")
                ],
            -- Ошибка базы данных: Указан несуществующий параметр "user_id": 666
            (,) ["authors", "1", "edit"] [
                    ("user_id", Just "666"),
                    ("description", Just "foo")
                ],
            -- {"edited":{"users":1}}
            (,) ["authors", "1", "edit"] [
                    ("user_id", Just "5"),
                    ("description", Just "foo")
                ],
            -- {"edited":{"users":1}}
            (,) ["authors", "1", "edit"] [
                    ("description", Just "bar")
                ],
            (,) ["authors"] []
        ]

deleteAuthorCases :: (String, [(PathInfo, Query)])
deleteAuthorCases = ("deleteAuthor", tuples) where
    tuples = [
            (,) ["authors"] [],
            (,) ["posts"] [],
            (,) ["authors", "0", "delete"] [], -- {}
            (,) ["authors", "1", "delete"] [], -- Ошибка базы данных: Невозможно удалить автора по умолчанию с id = 1
            (,) ["authors", "4", "delete"] [], -- {"edited": {"posts": 1},"deleted": {"authors": 1}}
            (,) ["authors"] [],
            (,) ["posts"] []
        ]

tagCases :: (String, [(PathInfo, Query)])
tagCases = ("insertTag", queries) where
    --pathInfos = repeat ["tags", "create"]
    queries = [
            (,) ["tags"] [],

            --Ошибка веб-запроса: Не указан обязательный параметр "name"
            (,) ["tags", "create"] [],

            --Ошибка веб-запроса: Не указано значение параметра "name"
            (,) ["tags", "create"][("name", Nothing)],

            -- {"created":{"tags":1}}
            (,) ["tags", "create"][("name", Just "some_tag")],              (,) ["tags"] [],

            --Ошибка базы данных: Тег с таким "name" = "some_tag" уже существует
            (,) ["tags", "create"][("name", Just "some_tag")],

            (,) ["tags", "10", "edit"][("name", Just "some_new_tag")],         (,) ["tags"] [],

            (,) ["posts"] [],

            --каскадное удаление (удаляются привязки к посту и черновику)
            (,) ["tags", "2", "delete"] [],         (,) ["tags"] [],   (,) ["posts"] []

        ]

authTagCases :: (String, [(PathInfo, Query)])
authTagCases = ("insertTag", queries) where
    --pathInfos = repeat ["tags", "create"]
    queries = [
            -- Ошибка авторизации: Неверный логин или пароль!
            (,) ["login"][("login", Just "login"),("pass", Just "pass")],

            -- Ошибка веб-запроса: Неизвестный путь: "[\"tags\",\"create\"]"
            (,) ["tags", "create"][("name", Just "some_tag")],

            -- {"token": "3_user_2021-03-23_abc..."}
            (,) ["login"][("login", Just "pivan"),("pass", Just "equalpass")],

            -- Ошибка веб-запроса: Неизвестный путь: "[\"tags\",\"create\"]"
            (,) ["tags", "create"][("name", Just "some_tag")],

            -- {"token": "2_admin_2021-03-23_d2d..."}
            (,) ["login"][("login", Just "admin"),("pass", Just "123456")],

            -- {"created":{"tags":1}}
            (,) ["tags", "create"][("name", Just "some_tag")],

            (,) ["tags"] []
        ]

insertCategoryCases :: (String, [(PathInfo, Query)])
insertCategoryCases = ("insertCategory", zip pathInfos queries) where
    pathInfos = repeat ["categories", "create"]
    queries = [
            --Ошибка веб-запроса: Не указан обязательный параметр "category_name"
            [],

            --Ошибка базы данных: Указан несуществующий параметр "parent_id": 666
            [("parent_id", Just "666"),
            ("category_name", Just "description for category")],

            --КОРРЕКТНЫЙ ЗАПРОС
            [("parent_id", Just "8"),
            ("category_name", Just "description for category")],

            --КОРРЕКТНЫЙ ЗАПРОС
            [("parent_id", Just "8"),
            ("category_name", Just "description for category")],

            --КОРРЕКТНЫЙ ЗАПРОС (КОРНЕВАЯ КАТЕГОРИЯ)
            [("category_name", Just "description2 for category")],

            --Ошибка веб-запроса: Не указано значение параметра "parent_id"
            [("parent_id", Nothing),
            ("category_name", Just "description for category")]
        ]

updateCategoryCases :: (String, [(PathInfo, Query)])
updateCategoryCases = ("updateCategory", tuples) where
    tuples = [
            (,) ["categories"] [],

            -- Ошибка веб-запроса: Необходимо указать хотя бы один параметр для редактирвания из следующего списка : ["category_name","parent_id"]
            (,) ["categories", "0", "edit"] [],

            -- Ошибка базы данных: Указан несуществующий параметр "id": 0
            (,) ["categories", "0", "edit"] [
                    ("parent_id", Just "5"),
                    ("category_name", Just "foo")
                ],
            -- Ошибка базы данных: Отсутствует категория 666
            (,) ["categories", "1", "edit"] [
                    ("parent_id", Just "666"),
                    ("category_name", Just "foo")
                ],
            -- Ошибка базы данных: Категрия 7 имеет в списке родителей [1,5,6] категорию 1. Невозможно создать циклическую категорию
            (,) ["categories", "1", "edit"] [
                    ("parent_id", Just "7"),
                    ("category_name", Just "foo")
                ],
            -- {"edited":{"users":1}}
            (,) ["categories", "7", "edit"] [
                    ("category_name", Just "bar")
                ],
            -- {"edited":{"users":1}}
            (,) ["categories", "7", "edit"] [
                    ("parent_id", Just "null")
                ],
            (,) ["categories"] []
        ]


deleteCategoryCases :: (String, [(PathInfo, Query)])
deleteCategoryCases = ("deleteCategory", tuples) where
    tuples = [
            (,) ["login"] [("login", Just "admin"),("pass", Just "123456")],
            (,) ["categories"] [],
            (,) ["categories", "0", "delete"] [], -- {}
            (,) ["categories", "1", "delete"] [],
            (,) ["categories", "2", "delete"] [],

            --редактируем черновик
            (,) ["drafts", "1","edit"] [
                ("category_id", Just "4")
                ],

            (,) ["categories", "2", "delete"] [],

            --редактируем пост
            (,) ["posts", "2","edit"] [
                    ("name", Just "edited_name2"),
                    ("category_id", Just "4"),
                    ("text", Just "edited_text2"),
                    ("photo", Just "edited_photo2.jpg")
                ],

            (,) ["drafts"] [],

            --и публикуем
            (,) ["drafts", "2","publish"] [],

            --и пытаемся снова.. успех! да, удалить категорию непросто
            (,) ["categories", "2", "delete"] [],
            (,) ["categories", "8", "delete"] [],
            (,) ["categories"] []
        ]

userCases :: (String, [(PathInfo, Query)])
userCases = ("user", tuples) where
    tuples = [
            --CREATE--
            (,) ["users"] [], --только для админа
            (,) ["user"] [],  --для авторизованного
            (,) ["users", "3"] [],  --только для админа
            -- {"created":{"users":1}}
            (,) ["users", "create"] [
                    ("last_name", Just "last_name"),
                    ("first_name", Just "first_name"),
                    ("avatar", Just "avatar"), --подумать над загрузкой фото
                    ("login", Just "login"),
                    ("pass", Just "pass")
                ],
            -- Ошибка базы данных: Пользователь с таким "login" = "login" уже существует
            (,) ["users", "create"] [
                    ("last_name", Just "last_name"),
                    ("first_name", Just "first_name"),
                    ("avatar", Just "avatar"), --подумать над загрузкой фото
                    ("login", Just "login"),
                    ("pass", Just "pass")
                ],


            --EDIT--
            (,) ["users"] [],
            -- Ошибка веб-запроса: Необходимо указать хотя бы один параметр для редактирвания из следующего списка : ["avatar","first_name","last_name","pass"]
            (,) ["users", "0", "edit"] [],
            -- Ошибка базы данных: Указан несуществующий параметр "id": 0
            (,) ["users", "0", "edit"] [("last_name", Just "new_last_name")],
            (,) ["users", "8", "edit"] [("last_name", Just "new_last_name")],
            (,) ["users", "8", "edit"] [("pass", Just "new_pass")],


            -- 11
            (,) ["users", "8", "edit"] [
                    ("last_name", Just "foo"),
                    ("first_name", Just "bar"),
                    ("avatar", Just "foo.jpg"),
                    ("pass", Just "foo")
                ],

            (,) ["login"] [("login", Just "login"),("pass", Just "foo")],  --проверка логина после редактирвоания - работает

            (,) ["user", "edit"] [
                    ("last_name", Just "edited"),
                    ("first_name", Just "edited"),
                    ("avatar", Just "edited.jpg")
                ],
            (,) ["users"] [],
            (,) ["authors"] [],
            -- (,) ["posts"] [],
            --DELETE--16
            (,) ["users", "0", "delete"] [], -- {}
            (,) ["users", "1", "delete"] [], -- Ошибка базы данных: Невозможно удалить пользователя по умолчанию с id = 1
            (,) ["users", "2", "delete"] [], -- Ошибка базы данных: Невозможно удалить админа с id = 2
            (,) ["users", "7", "delete"] [], -- {"edited": {"authors": 1},"deleted": {"users": 1}}
            (,) ["users"] [],
            -- (,) ["authors"] [],
            (,) ["posts"] []
        ]

deletePostCases :: (String, [(PathInfo, Query)])
deletePostCases = ("deletePost", tuples) where
    tuples = [
            (,) ["posts"] [],
            (,) ["posts", "0", "delete"] [], -- {}
            (,) ["posts", "1", "delete"] [], -- {"deleted": {"photos": 2,"comments": 3,"posts": 1}}
            (,) ["posts"] [],
            (,) ["posts", "2", "delete"] [], -- {"deleted": {"posts": 1}}
            (,) ["posts"] []
        ]

commentsCases :: (String, [(PathInfo, Query)])
commentsCases = ("selectComment", tuples) where
    tuples = [
            (,) ["posts", "2", "comments"] [], --[]
            (,) ["posts", "1", "comments"] [],

            -- Ошибка базы данных: Указан несуществующий параметр "post_id": 666
            (,) ["posts", "666", "comments", "create"] [
                --("user_id", Just "2"),
                ("text", Just "Some new comment to wrong post")
                ],

            -- {"created": {"comments": 1}}
            (,) ["posts", "1", "comments", "create"] [
                ("text", Just "Some new comment from right user")
                ],
            (,) ["posts", "1", "comments"] [],

            -- {"edited":{"comments":1},"deleted":{"users":1}}
            (,) ["users", "3", "delete"] [],        (,) ["posts", "1", "comments"] [],

            -- {"deleted":{"comments":1}}
            (,) ["comments", "2", "delete"] [],     (,) ["posts", "1", "comments"] [],

            -- {"deleted":{"comments":1}}
            (,) ["comments", "6", "delete"] [],     (,) ["posts", "1", "comments"] [],

            -- {"deleted":{"photos":2,"comments":3,"posts":1}}
            (,) ["posts", "1", "delete"] [],        (,) ["posts", "1", "comments"] []
        ]

--жизненный цикл новости
publishCases :: (String, [(PathInfo, Query)])
publishCases = ("publish", tuples) where
    tuples = [
        -- -- 0
        -- (,) ["authors", "3", "delete"] [], -- {"edited": {"posts": 1},"deleted": {"authors": 1}}

        -- -- 1 Ошибка авторизации: Данная функция требует авторизации; []; успех
        -- (,) ["drafts"] [],
        -- -- 2 null, null, null
        -- (,) ["drafts", "0"] [],
        -- -- 3 null, null, null
        -- (,) ["drafts", "1"] [],
        -- -- 4 успех, успех, успех
        -- (,) ["posts"] [],

        -- -- 5 Ошибка базы данных: Указан несуществующий параметр "category_id": 0
        -- (,) ["drafts", "create"] [
        --     ("name", Just "name"),
        --     ("category_id", Just "0"),
        --     ("text", Just "text"),
        --     ("photo", Just "photo.jpg")
        -- ],

        -- 6 {"created":{"contents":1,"drafts":1,"photos":2}}
        (,) ["drafts", "create"] [
            ("name", Just "name"),
            ("category_id", Just "1"),
            ("text", Just "text"),
            ("photo", Just "photo.jpg"),
            ("tag_id__all", Just "[]"),
            ("photos__all", Just "[\"photo1.jpg\",\"photo2.jpg\"]")
        ],
        -- 7 Ошибка базы данных: Параметры "tag_id" из списка [666,3589] не существуют
        (,) ["drafts", "create"] [
            ("name", Just "name"),
            ("category_id", Just "1"),
            ("text", Just "text"),
            ("photo", Just "photo.jpg"),
            ("tag_id__all", Just "[1,2,666,3589,3]"),
            ("photos__all", Just "[\"photo1.jpg\",\"photo2.jpg\"]")
        ],
        -- 8 {"created":{"contents":1,"drafts":1,"photos":2}}
        (,) ["drafts", "create"] [
            ("name", Just "name"),
            ("category_id", Just "1"),
            ("text", Just "text"),
            ("photo", Just "photo.jpg"),
            ("tag_id__all", Just "[1,2,3]"),
            ("photos__all", Just "[\"photo1.jpg\",\"photo2.jpg\"]")
        ],

        (,) ["drafts"] [],
        -- Ошибка базы данных: Указан несуществующий параметр "id": 0
        (,) ["drafts", "0","edit"] [
            ("category_id", Just "2")
        ],
        -- 8 Ошибка базы данных: Указан несуществующий параметр "category_id": 0
        (,) ["drafts", "2","edit"] [
            ("category_id", Just "0")
        ],
        -- 9 {"edited":{"contents":1}}
        (,) ["drafts", "2","edit"] [
            ("category_id", Just "2")
        ],
        -- 10 {"created":{"photos":2},"edited":{"contents":1},"deleted":{"photos":2}}
        (,) ["drafts", "2","edit"] [
            ("name", Just "edited_name"),
            ("category_id", Just "3"),
            ("text", Just "edited_text"),
            ("photo", Just "edited_photo.jpg"),
            ("tag_id__all", Just "[4,5,6]"),
            ("photos__all", Just "[\"newphoto1.jpg\",\"newphoto2.jpg\"]")
        ],  --РАБОТАЕТ 03.04.2021

        (,) ["drafts"] [],
        -- 11 {"created":{"posts":1},"deleted":{"drafts":1}} --работает
        (,) ["drafts", "2","publish"] [],
        -- 12 {"deleted":{"contents":1,"drafts":1,"photos":2}}
        (,) ["drafts", "3","delete"] [], --работает
        -- 13
        (,) ["posts"] [], --работает
        -- 14 {"created":{"contents":1,"drafts":1}}
        (,) ["posts", "3","edit"] [
            --("author_id", Just "3"), --авторство никогда не меняется
            ("name", Just "edited_name2"),
            ("category_id", Just "4"),
            ("text", Just "edited_text2"),
            ("photo", Just "edited_photo2.jpg"),
            ("tag_id__all", Just "[7,8]"),
            ("photos__all", Just "[\"newphoto3.jpg\",\"newphoto4.jpg\"]")
        ],
        -- 15
        (,) ["drafts"] [],
        -- 16 {"edited":{"contents":1}}
        (,) ["drafts", "4","edit"] [
            --("author_id", Just "3"), --авторство никогда не меняется
            ("name", Just "edited_name2"),
            ("category_id", Just "4"),
            ("text", Just "edited_text2"),
            ("photo", Just "edited_photo2.jpg")
        ],
        -- 17
        --(,) ["drafts", "4","delete"] [],
        -- 18
        (,) ["posts"] [],
        -- 19 {"edited":{"posts":1},"deleted":{"contents":1,"drafts":1,"photos":2}}
        (,) ["drafts", "4","publish"] [],
        -- 20
        (,) ["posts", "3"] [],
        -- 21 {"deleted":{"contents":1,"drafts":1,"photos":2,"posts":1}}
        (,) ["posts", "3", "delete"] []
        ]

--Каждый тест проходит с каждым из логинов
logins :: [Query]
logins = [
        -- [("login", Just "fake"),("pass", Just "fake")],  --не авторизован
        -- --[("login", Just "DELETED_USER"),("pass", Just "DELETED_USER")],  --удаленный
        -- [("login", Just "pivan"),("pass", Just "equalpass")],  -- иванов (не автор)
        -- [("login", Just "vmayakovskiy"),("pass", Just "vmayakovskiypass")],  --удаленный автор
        [("login", Just "psergey"),("pass", Just "psergeypass")],  --пушкин (автор)
        [("login", Just "admin"),("pass", Just "123456")]  --админ
    ]

casesWithAuth :: (String, [(PathInfo, Query)]) -> (String, [(PathInfo, Query)])
casesWithAuth cases = (,) (fst cases) $ concat $ for (snd cases) $ \c -> concat $ for logins $ \login -> [(["login"], login), c]


cases :: [(String, [(PathInfo, Query)])]
cases = [
    -- selectPostCases,
    -- casesWithAuth insertAuthorCases,
    -- updateAuthorCases,
    -- casesWithAuth deleteAuthorCases,
    -- casesWithAuth tagCases,
    -- insertCategoryCases,
    -- updateCategoryCases,
    -- deleteCategoryCases,
    --casesWithAuth userCases,
    --deletePostCases,
    --casesWithAuth commentsCases,
    casesWithAuth publishCases,
    --authTagCases,
    ("fake", [])
    ]



-- --ВНИМАНИЕ!!! Данная функция для корректного тестирования сбрасывает БД до изначального состояния!
--отслеживать выходной json можно в файле response.json (vscode обновляет автоматически)
emul :: IO ()
emul = runT $ do
    Log.debugOff
    Migrations.dbrestartForce  --сброс БД!!!
    Log.debugOn
    forM_ cases $ uncurry listOfTestCasesByOne
    Log.infoCM Color.Blue "Все запросы завершены..."

--нужен рефакторинг! отдельная ветка для логина, и отдельная для другого запроса
listOfTestCasesByOne :: MT m => String -> [(PathInfo, Query)] -> m (Maybe Token, Maybe Int)
listOfTestCasesByOne name qs = do
    Log.infoCM Color.Yellow  " Нажмите Enter для начала теста..."
    readLnT
    forMMem (zip [1,2..] qs) (Nothing, Nothing) $ \(mt, mn) (n, (pathInfo, query)) -> do
        Error.catch (do

            liftEIO clearScreen
            --Log.debugT (mt, mn)
            Log.infoCM Color.Blue  $ template "Проверка {1}, тестовый случай {0}: " [show n, name]
            Log.debugM (pathInfo, query)
            Log.infoCM Color.Cyan $ template "Token: {0}" [show mt]

            --query в тестах для простоты дублируется и в строке запроса и в теле запроса.
            headers <- case mt of
                Nothing        -> return []
                Just (Token t) -> return [("Authorization", convert t)]
            newmt <- case pathInfo of
                ["login"] -> do
                    Log.debugOff
                    --str <- Error.catch (DB.getJSONTest (convert $ show pathInfo) pathInfo query query headers) (\e -> return "")
                    Error.catch (do
                        str <- Response.getJSONTest (convert $ show pathInfo) pathInfo query query headers
                        --tmp <- toT . (Just <$>) . typeError ParseError . eitherDecode $ str
                        tmp <- Just <$> Error.catchEither (eitherDecode str) ParseError
                        Log.debugOn
                        Log.infoCM Color.Green $ template "Аутентификация успешно завершена, токен: {0} . Нажмите Enter для следующего теста, q + Enter для выхода или номер_теста + Enter..." [show tmp]
                        return tmp
                        )
                        (\e -> do
                        Log.debugOn
                        Log.infoCM Color.Yellow $ show e
                        return Nothing)
                _ -> do
                    Response.getJSONTest (convert $ show pathInfo) pathInfo query query headers
                    Log.infoCM Color.Green "Запрос успешно завершен. Нажмите Enter для следующего теста, q + Enter для выхода или номер_теста + Enter..."
                    
                    return mt
            newmn <- readCommand n mn
            Log.debugM newmt
            return (newmt, newmn)
            ) $ \e -> do
                case e of
                    IOError _ -> Error.throw e
                    _ -> do
                        Log.infoCM Color.Yellow "Запрос НЕуспешно завершен."
                        Log.infoCM Color.Yellow $ show (e::E)
                        Log.infoCM Color.Yellow "Нажмите Enter для следующего теста, q + Enter для выхода или номер_теста + Enter..."
                        newmn <- readCommand n mn
                        return (mt, newmn)


    where
        readCommand :: MIOError m => Int -> Maybe Int -> m (Maybe Int)
        readCommand n mn = if Just n < mn  then return mn else do
            answ <- readLnT
            when (answ == "q") $ Error.throw $ IOError "Выход из теста по требованию пользователя"
            case readEither answ of
                Right newn -> return (Just newn)
                _          -> return Nothing


