module App.Main where

-- Our Modules
import qualified App.Server          as Server
import qualified Logic.DB.Migrations as Migrations
import           T.State             (T)
import           T.Transformer       (runT)

-- Other Modules
import           System.Environment  (getArgs)

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

-- Опции командной строки:
-- Запуск без аргументов    - запуск сервера
-- db-init                  - инициализация всех таблиц базы данных с нуля
-- migrations               - применение миграций к локальной базе данных без потери данных
-- db-drop                  - удаление всех таблиц БД

main :: IO()
main = do
    args <- getArgs
    case args of
        [] -> server
        ["db-init"] -> dbinit
        ["migrations"] -> migrations
        ["db-drop"] -> dbdrop
        _ -> do
            putStrLn "Неверные опции командной строки!"
            putStrLn "Возможные опции:"
            putStrLn "Запуск без аргументов       - запуск сервера"
            putStrLn "db-init                     - инициализация всех таблиц базы данных с нуля"
            putStrLn "migrations                  - применение миграций к локальной базе данных без потери данных"
            putStrLn "db-drop                     - удаление всех таблиц БД"

dbdrop :: IO ()
dbdrop = runT Migrations.dbdrop

migrations :: IO ()
migrations = runT Migrations.run 

dbinit :: IO ()
dbinit = runT Migrations.dbinit 

server :: IO ()
server = Server.run
