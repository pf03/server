module App.Main where

import qualified App.Server as Server
import qualified Logic.DB.Migrations.Functions as Migrations
import qualified Logic.IO.Emulate as Emulate
import System.Environment (getArgs)
import qualified Transformer.Exports as Transformer

main_ :: IO ()
main_ = do
  args <- getArgs
  case args of
    [] -> serverRun_
    ["db-init"] -> dbInit_
    ["migrations"] -> migrationsRun_
    ["db-drop"] -> dbDrop_
    ["db-restart"] -> dbRestart_
    ["db-restart-force"] -> dbRestartForce_
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
  return ()

dbInit_ :: IO ()
dbInit_ = Transformer.runT Migrations.dbInit

migrationsRun_ :: IO ()
migrationsRun_ = Transformer.runT Migrations.run

dbDrop_ :: IO ()
dbDrop_ = Transformer.runT Migrations.dbDrop

serverRun_ :: IO ()
serverRun_ = Server.run

dbRestart_ :: IO ()
dbRestart_ = dbDrop_ >> dbInit_

dbRestartForce_ :: IO ()
dbRestartForce_ = Transformer.runT Migrations.dbRestartForce

genTokens_ :: IO ()
genTokens_ = Transformer.runT Emulate.writeTokens