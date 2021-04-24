module Logic.DB.Migrations where

-- Our Modules
import           Common.Misc
import           Interface.DB                     as DB
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

-----------------------------External--------------------------------
dbinit :: MDB m => m ()
dbinit = do
    Log.warnM "Производятся инициализация базы данных с нуля..."
    migrate namesList
    Log.warnM "Все миграции выполнены успешно"

run :: MDB m => m ()
run = do
    Log.warnM "Производятся миграции базы данных..."
    migrations <- Select.allMigrations
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

namesList :: [FileName]
namesList = [
    "0000_base.sql",
    "0001_insert_data_en.sql",
    "0002_hash_passwords.sql",
    "0003_rename_news_to_posts.sql"
    ]

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
