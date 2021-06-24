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

dbInit :: MDB m => m ()
dbInit = do
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

dbDrop :: MDB m => m ()
dbDrop = do
  Log.warnM "Warning! All database tables will be dropped Y/N"
  answer <- readLnT
  case answer of
    "N" -> Log.infoM "Exit from the migration program"
    "Y" -> dbDropForce
    _ -> Log.infoM "Wrong choice. Try again"

dbDropForce :: MDB m => m ()
dbDropForce = do
  executeFile $ pathMigration "drop.sql"
  Log.warnM "All database tables are dropped by user choice"

dbRestartForce :: MDB m => m ()
dbRestartForce = do
  dbDropForce
  dbInit