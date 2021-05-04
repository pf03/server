module Logic.DB.Migrations where

-- Our modules
import           Common.Misc
import           Interface.DB                     as DB hiding (all)
import           Interface.Error                  as Error
import           Interface.Log                    as Log
import qualified Logic.DB.Insert                  as Insert
import           Logic.DB.Row                     as Row
import           Logic.DB.Select                  as Select
import qualified Logic.IO.File                    as File

-- Other modules
import           Control.Monad
import           Database.PostgreSQL.Simple.Types
import qualified System.Console.ANSI              as Color (Color (..))
import System.Directory
import Data.List

-----------------------------External--------------------------------
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
        _   -> Log.infoM "Wrong choice. Try again"

dbdropForce :: MDB m => m ()
dbdropForce = do
    executeFile $ pathMigration "drop.sql"
    Log.warnM "All database tables are dropped by user choice"

dbrestartForce :: MDB m => m ()
dbrestartForce = do
    dbdropForce
    dbinit

-----------------------------Internal------------------------------------------
pathMigrations :: FilePath
pathMigrations = "migrations"

pathMigration :: FileName -> FilePath
pathMigration name = template "{0}/{1}" [pathMigrations, name]

getNamesList :: MIOError m => m [FileName]
getNamesList = do
    items <- liftEIO $ listDirectory pathMigrations
    printT items
    let files = sort $ filter helper items
    unless (all (uncurry helper2) $ zip [0..] files) $ Error.throw $ IOError "Check the order of numbers in migration files!"
    printT files
    return files
    where
        -- Check format 0000_migration_name.sql
        helper :: FileName -> Bool
        helper (a:b:c:d:'_':xs)|all (`elem` show[0..9]) [a,b,c,d] && ".sql" `isSuffixOf` xs = True 
        helper _ = False

        -- Check order of file names, n <= 9999
        helper2 :: Int -> FileName -> Bool
        helper2 n f = shown `isPrefixOf` f where
            shown = replicate (4 - length (show n)) '0' <> show n

-- | From the entire list of migrations, we choose those that have not yet been applied
checkMigrations :: (MError m, MLog m) => [Select.Migration] -> [FileName] -> m [FileName]
checkMigrations migrations names = do
    zipWithM_ helper migrations names -- Checking migration names match
    return $ drop (length migrations) names
    where
        helper migration name = if migrationName migration == name
            then Log.infoM $ template "Migration {0} is already applied..." [name]
            else Error.throw $ DBError $ template "Database migration name: {0} does not match the file name: {1}" [migrationName migration, name]

migrate :: MDB m => [FileName] -> m ()
migrate = mapM_ $ \name -> do
    Log.warnM $ template "Migration in progress: {0}..." [name]
    executeFile $ pathMigration name
    Insert.migration name

-- | Exequte sql from file
executeFile :: MDB m => FilePath -> m ()
executeFile path = do
    queryBS <- File.read path
    let query = Query queryBS
    DB.execute_ query []
