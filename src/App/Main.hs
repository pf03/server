module App.Main where

-- Our modules
import qualified App.Server          as Server
import qualified Logic.DB.Migrations as Migrations
import qualified Logic.IO.Emulate       as Emulate
import           T.Transformer       (runT)

-- Other modules
import           System.Environment  (getArgs)

main :: IO ()
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
            putStrLn "Wrong command line arguments!"
            putStrLn "Possible arguments:"
            putStrLn "start with no arguments   - start the server"
            putStrLn "db-init                   - applying migrations to the local database, starting with zero migration"
            putStrLn "migrations                - applying migrations to the local database, starting with the first not applied migration (the one that is not in the migrations database table)"
            putStrLn "db-drop                   - drop all database tables"
            putStrLn "db-restart                - ‘db-drop’ + ‘db-init’"
            putStrLn "db-restart-force          - force ‘db-restart’"
            putStrLn "gen-tokens                - update token file ‘dist/curl/tokens.sh’"
    putStrLn "Press Enter for exit..."
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
