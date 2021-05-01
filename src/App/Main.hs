module App.Main where

-- Our Modules
import qualified App.Server          as Server
import qualified Logic.DB.Migrations as Migrations
import           T.State             (T)
import           T.Transformer       (runT)
import App.Emulate as Emulate

-- Other Modules
import           System.Environment  (getArgs)

-- Это все перенести в Readme и перевести на английский
-- ПОРЯДОК ЗАПУСКА И ТЕСТИРОВАНИЯ (проверено только для Windows):
-- 1. 
-- git clone https://github.com/pf03/server.git
-- cd server
-- stack build
-- 2. Полученный испольняемый файл (в случае Windows server-exe.exe) переместите в папку dist репозитория, 
-- которая содержит все необходимые дополнительные файлы
-- 3. Файл config-example.json переименуйте в config.json и при необходимости заполните
-- 4. Для создания таблиц базы данных с нуля и приведение в актуальное состояние запустите сервер с флагом db-init.
-- Остальные опции командной строки, которые могут пригодиться во время тестирования, указаны ниже.
-- 3. Для обновления файла токенов dist/curl/tokens.sh запустите сервер с флагом gen-tokens
-- 4. api-функции тестируются с помощью curl-запросов в соответствующей папке
-- 5. Для тестирования чистых функций запустите stack test
-- 6. Для тестирования миграций:
--    названия файлов имеют строгий формат 1234_migration_name.sql и строгую очередность, начиная с 0000_migration_name.sql;
--    файл drop.sql используется для удаления таблиц;
--    файлы с другими названиями игнорируются.
-- Для тестирования необходимо 
--    удалить БД с помощью команды db-drop, 
--    удалить (переименовать) часть файлов миграций,
--    инициализировать таблицы и применить оставшиеся в папке миграции командой db-init,
--    вернуть миграции в папку migrations
--    применить все миграции с помощью команды migrations

-- Для запуска исполняемого файла нео

-- ОПЦИИ КОМАНДНОЙ СТРОКИ:

-- ЗАПУСК СЕРВЕРА:
-- Запуск без аргументов    - запуск сервера
-- translit                 - сообщения об ошибках на транслите, из-за проблем с кириллицей в bash (перевести на английский!!!!!!!!!!!!!)

-- МИГРАЦИИ:
-- db-init                  - применение миграций к локальной базе данных, начиная с нулевой миграции
-- migrations               - применение миграций к локальной базе данных, начиная с первой невыполненной миграции (той, которой нет в таблице БД migrations)
-- db-drop                  - удаление всех таблиц БД
-- db-restart               - db-drop + db-init
-- db-restart-force         - то же принудительно

-- CURL:
-- gen-tokens               - Обновление файла токенов dist/curl/tokens.sh


-- stack test

-- Логика работы сервера разбита на следующие слои (представлены в соответствующих папках в src):
-- 1. Common    - общие функции;
-- 2. Interface - классы типов, которые реализуют абстрактный доступ высших слоев к интерфейсам:
--      MError  - обработки ошибок,
--      MLog    - логгирования,
--      MCache  - работы с изменяющимися данными,
--      MDB     - работы с базой данных PostgreSQL;
-- 3. Logic      - основная логика работы программы, для удобства разбита на:
--      Pure    - чистые функции,
--      IO      - IO-функции,
--      DB      - функции для работы с базой данных;
-- 4. T         - одна из возможных реализаций интерфейса - трансформер T;
-- 5. App       - слой приложения - функции, которые имеют доступ как к интерфейсу, так и к его реализации.
-- Низшие слои не могут импортировать модули из высших слоев.



main :: IO()
main = do
    args <- getArgs
    case args of
        [] -> server_
        ["db-init"] -> dbinit_
        ["migrations"] -> migrations_
        ["db-drop"] -> dbdrop_
        ["db-restart"] -> dbrestart_
        ["db-restart-force"] -> dbrestartForce_
        ["gen-tokens"] -> genTokens_
        _ -> do
            putStrLn "Неверные опции командной строки!"
            putStrLn "Возможные опции:"
            putStrLn "Запуск без аргументов - запуск сервера"
            putStrLn "db-init               - применение миграций к локальной базе данных, начиная с нулевой миграции"
            putStrLn "migrations            - применение миграций к локальной базе данных, начиная с первой невыполненной миграции (той, которой нет в таблице БД migrations)"
            putStrLn "db-drop               - удаление всех таблиц БД"
            putStrLn "db-restart            - db-drop + db-init"
            putStrLn "db-restart-force      - то же принудительно"
            putStrLn "gen-tokens            - Обновление файла токенов dist/curl/tokens.sh"
    putStrLn "Нажмите Enter для выхода из приложения..."
    _ <- getLine
    return()



dbinit_ :: IO ()
dbinit_ = runT Migrations.dbinit 

migrations_ :: IO ()
migrations_ = runT Migrations.run 

dbdrop_ :: IO ()
dbdrop_ = runT Migrations.dbdrop

server_ :: IO ()
server_ = Server.run

dbrestart_ :: IO ()
dbrestart_ = dbdrop_ >> dbinit_

dbrestartForce_ :: IO ()
dbrestartForce_ = runT Migrations.dbrestartForce 

genTokens_ :: IO()
genTokens_ = runT Emulate.writeTokens


