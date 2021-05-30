module Logic.DB.Migrations.Functions where

import Logic.DB.Migrations.Internal
    ( pathMigration,
      getNamesList,
      checkMigrations,
      migrate,
      executeFile )
import Interface.Class ( MDB )
import Common.Functions ( readLnT )
import qualified Interface.MLog.Exports as Log
import qualified Logic.DB.Select.Exports as Select

dbinit :: MDB m => m ()
dbinit = do
  Log.warnM "Database initialization and migrations in progress..."
  namesList <- getNamesList
  migrate namesList
  Log.warnM "Database initialization and all migrations completed successfully"

run :: MDB m => m ()
run = do
  Log.warnM "Migrations in progress..."
  migrations <- Select.allMigrations
  namesList <- getNamesList
  namesTodo <- checkMigrations migrations namesList
  migrate namesTodo
  Log.warnM "All migrations completed successfully"

dbdrop :: MDB m => m ()
dbdrop = do
  Log.warnM "Warning! All database tables will be dropped Y/N"
  answer <- readLnT
  case answer of
    "N" -> Log.infoM "Exit from the migration program"
    "Y" -> dbdropForce
    _ -> Log.infoM "Wrong choice. Try again"

dbdropForce :: MDB m => m ()
dbdropForce = do
  executeFile $ pathMigration "drop.sql"
  Log.warnM "All database tables are dropped by user choice"

dbrestartForce :: MDB m => m ()
dbrestartForce = do
  dbdropForce
  dbinit