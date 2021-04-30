module Logic.DB.Migrations where

-- Our Modules
import           Common.Misc
import           Interface.DB                     as DB hiding (all)
import           Interface.Error                  as Error
import           Interface.Log                    as Log
import qualified Logic.DB.Insert                  as Insert
import           Logic.DB.Row                     as Row
import           Logic.DB.Select                  as Select
import qualified Logic.IO.File                    as File

-- Other Modules
import           Control.Monad
import           Database.PostgreSQL.Simple.Types
import qualified System.Console.ANSI              as Color (Color (..))
import System.Directory
import Data.List

-----------------------------External--------------------------------
dbinit :: MDB m => m ()
dbinit = do
    Log.warnM "Производятся инициализация базы данных с нуля..."
    namesList <- getNamesList
    migrate namesList
    Log.warnM "Все миграции выполнены успешно"

run :: MDB m => m ()
run = do
    Log.warnM "Производятся миграции базы данных..."
    migrations <- Select.allMigrations
    namesList <- getNamesList
    namesTodo <- checkMigrations migrations namesList
    migrate namesTodo
    Log.warnM "Все миграции выполнены успешно"

dbdrop :: MDB m => m ()
dbdrop = do
    Log.warnM "Внимание! Все таблицы базы данных будут стерты Y/N"
    answer <- readLnT
    case answer of
        "N" -> Log.infoM "Выход из программы миграций"
        "Y" -> dbdropForce
        _   -> Log.infoM "Неверный выбор. Попробуйте снова"

dbdropForce :: MDB m => m ()
dbdropForce = do
    executeFile $ pathMigration "drop.sql"
    Log.warnM "Все таблицы БД стерты по выбору пользователя"

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
    unless (all (uncurry helper2) $ zip [0..] files) $ Error.throw $ IOError "Проверьте порядок номеров в файлах миграций!"
    printT files
    return files
    where
        --проверка формата 0000_migration_name.sql
        helper :: FileName -> Bool
        helper (a:b:c:d:'_':xs)|all (`elem` show[0..9]) [a,b,c,d] && ".sql" `isSuffixOf` xs = True 
        helper _ = False

        --проверка очередности файлов, n <= 9999
        helper2 :: Int -> FileName -> Bool
        helper2 n f = shown `isPrefixOf` f where
            shown = replicate (4 - length (show n)) '0' <> show n

-- | Из всего списка миграций опредаляем те, ктороый еще не выполнены
checkMigrations :: (MError m, MLog m) => [Select.Migration] -> [FileName] -> m [FileName]
checkMigrations migrations names = do
    zipWithM_ helper migrations names --проверка соответствия имен миграций
    return $ drop (length migrations) names
    where
        helper migration name = if migrationName migration == name
            then Log.infoM $ template "Миграция {0} уже применена..." [name]
            else Error.throw $ DBError $ template "Имя миграции в БД: {0} не соответствует имени файла: {1}" [migrationName migration, name]

migrate :: MDB m => [FileName] -> m ()
migrate = mapM_ $ \name -> do
    Log.warnM $ template "Производится миграция {0}:" [name]
    executeFile $ pathMigration name
    Insert.migration name

-- | Выполнить скрипт из файла
executeFile :: MDB m => FilePath -> m ()
executeFile path = do
    queryBS <- File.read path
    let query = Query queryBS
    DB.execute_ query []
