{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Migrations where

-- Our Modules
import           Common.Misc
import           Interface.DB                     as DB
import           Interface.Log                    as Log
import qualified Logic.IO.File                    as File
import           T.State
import           T.Transformer

-- Other Modules
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as L
import           Data.Int
import qualified Data.Map                         as M
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types
import qualified System.Console.ANSI              as Color (Color (..))
import Logic.DB.Select as Select
import Logic.DB.Row as Row
import Interface.Error as Error
import qualified Logic.DB.Insert as Insert

-- в конце каждого файла миграций добавлять следующий запрос: 
-- INSERT INTO migrations (name, description) VALUES ({0}, "Описание миграции");
-- вместо {0} скрипт автоматически подставляет имя файла
-- это необходимо для контроля версий



--использовать этот список
--list :: M.Map String (T())
-- list :: MDB m => [(String, m ())]
-- list = --M.fromList
--     [
--         ("0000 УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными", base),
--         ("0001 Хеширование паролей", hashPasswords),
--         ("0002 Переименование news в posts", renameNewsToPosts)
--     ]

pathMigrations :: FilePath
pathMigrations = "migrations"

pathMigration :: FileName -> FilePath
pathMigration name = template "{0}/{1}" [pathMigrations, name]

-- https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
-- System.Directory
--информацию о списке миграций можно считать прямо с папки. 
namesList :: [FileName]
namesList = [
    "0000_base.sql",
    "0001_insert_data.sql",
    "0002_hash_passwords.sql",
    "0003_rename_news_to_posts.sql"
    ]

--Если этот модуль не будет запускать трансформер, то можно переместить его в слой логики Logic.DB
-- run :: IO ()
-- run = runT (App.Migrations.all :: T())

-- init :: IO ()
-- init = runT (App.Migrations.base :: T())

-- data Migration = All | Init

--forMMem (zip [1,2..] qs) (Nothing, Nothing) $ \(mt, mn) (n, (pathInfo, query)) -> do

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

dbrestartForce :: MDB m => m ()
dbrestartForce = do
    dbdropForce
    dbinit
    
dbdropForce :: MDB m => m ()
dbdropForce = do
    executeFile $ pathMigration "drop.sql"
    Log.warnM "Все таблицы БД стерты по выбору пользователя"


-- Из всего списк миграций опредаляем те, ктороый еще не выполнены
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

-- Выполнить скрипт из файла
executeFile :: MDB m => FilePath -> m ()
executeFile path = do
    queryBS <- File.read path
    let query = Query queryBS
    DB.execute__ query []
