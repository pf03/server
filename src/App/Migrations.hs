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






--использовать этот список
--list :: M.Map String (T())
list :: MDB m => [(String, m ())]
list = --M.fromList
    [
        ("0000 УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными", base),
        ("0001 Хеширование паролей", hashPasswords),
        ("0002 Переименование news в posts", renameNewsToPosts)
    ]

list1 :: [(FilePath, String)]
list1 = [
    (,) "0000_base.sql"
        "Создание таблиц базы данных",
    (,) "0001_insert_data.sql"
        "Заполнение таблиц данными",
    (,) "0002_hash_passwords.sql"
        "Хеширование паролей",
    (,) "0003_rename_news_to_posts.sql"
        "Переименование news в posts"
    ]

--Если этот модуль не будет запускать трансформер, то можно переместить его в слой логики Logic.DB
run :: IO ()
run = runT (App.Migrations.all :: T())

init :: IO ()
init = runT (App.Migrations.base :: T())

data Migration = All | Init

--сделать возможность выбора номера миграции
all :: MDB m => m ()
all = do
    --сделать лог, который не пишется в файл логов, а только в консоль (сообщение пользователю)
    Log.setSettings Color.Yellow True
    Log.infoM "Внимание! Все таблицы базы данных будут стерты и перезаписаны тестовыми данными Y/N"
    answer <- readLnT
    case answer of
        "N" ->  Log.infoM "Выход из программы миграций"
        "Y" -> allForce
        _ -> do
            Log.infoM "Неверный выбор. Попробуйте снова"
            App.Migrations.all

allForce :: MDB m => m()
allForce = do
    Log.infoM "Производятся миграции..."
    Log.off
    mapM_ wrapper App.Migrations.list
    Log.on
    Log.infoM "Все миграции выполнены успешно."

wrapper :: MDB m => (String, m()) -> m()
wrapper (name, func) = do
    Log.infoCM Color.Blue "Производится следующая миграция:"
    Log.infoCM Color.Cyan name
    Log.setSettings Color.Cyan False
    func
    Log.infoCM Color.Green "Миграция окончена успешно"

pathMigrations :: FilePath
pathMigrations = "migrations/0000_base.sql"

-- Выполнить скрипт из файла
executeFile :: MDB m => FilePath -> m ()
executeFile path = do
    queryBS <- File.read path
    let query = Query queryBS
    DB.execute__ query []

--DB initialization from sql file
base :: MDB m => m ()
base = do
    queryBS <- File.read pathMigrations
    let query = Query queryBS
    DB.execute__ query []

--обработка ошибок при запросах к бд!
hashPasswords :: MDB m => m ()
hashPasswords = do
    users <- DB.query_ [sql|SELECT id, login, pass FROM users|]
    Log.infoM "Получен список пользователей..."
    rows <- DB.executeMany [sql|
        UPDATE users
        SET pass = md5(upd.lp)
        FROM (VALUES (?, ?)) as upd(id, lp)
        WHERE users.id = upd.id
    |] $ map (\(uid, l, p) -> (uid, l <> " " <> p)) (users :: [(Int, B.ByteString, B.ByteString)])
    Log.infoM "Пароли хешированы..."
    DB.execute_ [sql|
        ALTER TABLE users
            ALTER COLUMN pass TYPE VARCHAR (32)
    |] []
    
    --доработать!!!
    let s = [sql|
        UPDATE users
        SET pass = md5 (CONCAT_WS(' ', login, pass))
        FROM (VALUES (?, ?)) as upd(id, lp)
        |]

    Log.infoM "Урезана длина строки пароля до 32 символов..."



--это тоже можно запихнуть в файл
renameNewsToPosts :: MDB m => m ()
renameNewsToPosts = do
    DB.execute__ [sql|
        DROP TABLE IF EXISTS posts;
        ALTER TABLE news RENAME TO posts;
        ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
        ALTER TABLE comments RENAME COLUMN news_id TO post_id;
    |] []
