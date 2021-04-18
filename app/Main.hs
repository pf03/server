module Main where
import qualified App.Server as Server
import qualified App.Migrations as Migrations
import System.Environment
import T.Transformer
import T.State

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
-- db-drop                     - удаление всех таблиц БД

main :: IO()
main = do
    --return ()
    args <- getArgs
    print args
    -- runT $ (Migrations.dbdrop :: T())
    -- runT $ (Migrations.run :: T())
    -- runT $ (Migrations.dbinit :: T())
    case args of 
        [] -> Server.run
        ["db-init"] -> runT $ (Migrations.dbinit :: T())
        ["migrations"] -> runT $ (Migrations.run :: T())
        ["db-drop"] -> runT $ (Migrations.dbdrop :: T())
        _ -> do
            putStrLn "Неверные опции командной строки!"
            putStrLn "Возможные опции:"
            putStrLn "Запуск без аргументов       - запуск сервера"
            putStrLn "db-init                     - инициализация всех таблиц базы данных с нуля"
            putStrLn "migrations                  - применение миграций к локальной базе данных без потери данных"
            putStrLn "db-drop                     - удаление всех таблиц БД"

-- dbdrop = runT $ (Migrations.dbdrop :: T())
-- run = runT $ (Migrations.run :: T())
-- dbinit = runT $ (Migrations.dbinit :: T())