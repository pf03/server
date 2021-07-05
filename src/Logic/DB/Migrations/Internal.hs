module Logic.DB.Migrations.Internal where

import Common.Types ( FileName )
import Interface.Class ( MIOError, MError, MLog, MDB )
import Common.Functions ( Template(template), printT)
import Control.Monad ( unless, zipWithM_ )
import Data.List ( sort, isPrefixOf, isSuffixOf )
import Database.PostgreSQL.Simple.Types ( Query(Query) )
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Interface.MLog.Exports as Log
import qualified Logic.DB.Insert as Insert
import qualified Logic.DB.Row as Row
import qualified Logic.DB.Select.Exports as Select
import qualified Logic.IO.File as File
import System.Directory ( listDirectory )

pathMigrations :: FilePath
pathMigrations = "migrations"

pathMigration :: FileName -> FilePath
pathMigration name = template "{0}/{1}" [pathMigrations, name]

getNamesList :: MIOError m => m [FileName]
getNamesList = do
  items <- Error.liftEIO $ listDirectory pathMigrations
  printT items
  let files = sort $ filter checkFormat items
  unless (all (uncurry checkOrder) $ zip [0 :: Int ..] files) $
    Error.throwIO "Check the order of numbers in migration files!" []
  printT files
  return files
  where
    -- Check format of file names 0000_migration_name.sql
    checkFormat :: FileName -> Bool
    checkFormat (a : b : c : d : '_' : xs) | all (`elem` show [0 :: Int .. 9]) [a, b, c, d] && ".sql" `isSuffixOf` xs = True
    checkFormat _ = False

    -- Check order of file names, n <= 9999
    checkOrder :: Int -> FileName -> Bool
    checkOrder n f = showN `isPrefixOf` f
      where
        showN = replicate (4 - length (show n)) '0' <> show n

-- | From the entire list of migrations, we choose those that have not yet been applied
checkMigrations :: (MError m, MLog m) => [Select.Migration] -> [FileName] -> m [FileName]
checkMigrations migrations names = do
  zipWithM_ checkMigration migrations names -- Checking migration names match
  return $ drop (length migrations) names
  where
    checkMigration migration name =
      if Row.migrationName migration == name
        then Log.writeInfoM $ template "Migration {0} is already applied..." [name]
        else
          Error.throwDB"Database migration name: {0} does not match the file name: {1}" [Row.migrationName migration, name]

migrate :: MDB m => [FileName] -> m ()
migrate = mapM_ $ \name -> do
  Log.writeWarnM $ template "Migration in progress: {0}..." [name]
  executeFile $ pathMigration name
  Insert.insertMigration name

-- | Execute sql from file
executeFile :: MDB m => FilePath -> m ()
executeFile path = do
  queryBS <- File.read path
  let query = Query queryBS
  DB.execute_ query []