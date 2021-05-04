module App.Main where

-- Our modules
import qualified App.Server          as Server
import qualified Logic.DB.Migrations as Migrations
import           T.State             (T)
import           T.Transformer       (runT)
import App.Emulate as Emulate

-- Other modules
import           System.Environment  (getArgs)

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


