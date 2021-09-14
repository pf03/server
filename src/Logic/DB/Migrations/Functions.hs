module Logic.DB.Migrations.Functions where

import Control.Monad.IO.Class (MonadIO (liftIO))
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
  Log.setColorScheme Log.CyanScheme
  Log.writeInfoM "Database initialization and migrations in progress..."
  namesList <- getNamesList
  migrate namesList
  Log.writeInfoM "Database initialization and all migrations completed successfully"

run :: MDB m => m ()
run = do
  Log.setColorScheme Log.CyanScheme
  Log.writeInfoM "Migrations in progress..."
  migrations <- Select.selectAllMigrations
  namesList <- getNamesList
  namesTodo <- checkMigrations migrations namesList
  migrate namesTodo
  Log.writeInfoM "All migrations completed successfully"

dbDrop :: MDB m => m ()
dbDrop = do
  Log.setColorScheme Log.CyanScheme
  Log.writeInfoM "Warning! All database tables will be dropped Y/N"
  answer <- liftIO getLine
  case answer of
    "N" -> Log.writeInfoM "Exit from the migration program"
    "Y" -> dbDropForce
    _ -> Log.writeInfoM "Wrong choice. Try again"

dbDropForce :: MDB m => m ()
dbDropForce = do
  Log.setColorScheme Log.CyanScheme
  executeFile $ pathMigration "drop.sql"
  Log.writeWarnM "All database tables are dropped by user choice"

dbRestartForce :: MDB m => m ()
dbRestartForce = do
  dbDropForce
  dbInit