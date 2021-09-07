module Logic.DB.Migrations.Functions where

import Common.Functions (readLnT)
import Interface.Class (MDB)
import qualified Interface.MLog.Exports as Log
import Logic.DB.Migrations.Internal
  ( checkMigrations,
    executeFile,
    getNamesList,
    migrate,
    pathMigration,
  )
import qualified Logic.DB.Select.Exports as Select

dbInit :: MDB m => m ()
dbInit = do
  Log.writeWarnM "Database initialization and migrations in progress..."
  namesList <- getNamesList
  migrate namesList
  Log.writeWarnM "Database initialization and all migrations completed successfully"

run :: MDB m => m ()
run = do
  Log.writeWarnM "Migrations in progress..."
  migrations <- Select.selectAllMigrations
  namesList <- getNamesList
  namesTodo <- checkMigrations migrations namesList
  migrate namesTodo
  Log.writeWarnM "All migrations completed successfully"

dbDrop :: MDB m => m ()
dbDrop = do
  Log.writeWarnM "Warning! All database tables will be dropped Y/N"
  answer <- readLnT
  case answer of
    "N" -> Log.writeInfoM "Exit from the migration program"
    "Y" -> dbDropForce
    _ -> Log.writeInfoM "Wrong choice. Try again"

dbDropForce :: MDB m => m ()
dbDropForce = do
  executeFile $ pathMigration "drop.sql"
  Log.writeWarnM "All database tables are dropped by user choice"

dbRestartForce :: MDB m => m ()
dbRestartForce = do
  dbDropForce
  dbInit