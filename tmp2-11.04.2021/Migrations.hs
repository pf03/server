{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Migrations where

--import Database.PostgreSQL.Simple (Query(..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
-- import Crypto.Hash.MD5
import  qualified Data.ByteString as B
import Types
import qualified Log
import qualified System.Console.ANSI as Color (Color(..)) 
import qualified State as S
import qualified Data.ByteString.Lazy as L
import Control.Exception
import Data.Int
import Control.Monad
import qualified DB
import DB (MT, MDB)
import Common
import qualified Data.Map as M
import Transformer
import qualified File

--использовать этот список
--list :: M.Map String (T())
list :: MDB m => [(String, m ())]
list = --M.fromList 
    [
        ("0000 УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными", base),
        ("0001 Хеширование паролей", hashPasswords),
        ("0002 Переименование news в posts", renameNewsToPosts)
    ]


--сделать возможность выбора номера миграции
all :: MDB m => m()
all = do
    --сделать лог, который не пишется в файл логов, а только в консоль (сообщение пользователю) 
    Log.setSettings Color.Yellow True "application" 
    Log.textT Log.Warning "Внимание! Все таблицы базы данных будут стерты и перезаписаны тестовыми данными Y/N"
    answer <- readLnT 
    case answer of
        "N" ->  putStrLnT "Выход из программы миграций"
        "Y" -> Migrations.allForce   
        _ -> do
            putStrLnT "Неверный выбор. Попробуйте снова"
            Migrations.all 

allForce :: MDB m => m()
allForce = do
    Log.colorTextT Color.Blue Log.Info "Производятся миграции..."
    Log.off
    mapM_ Migrations.wrapper Migrations.list
    Log.on
    Log.colorTextT Color.Green Log.Info "Все миграции выполнены успешно."

wrapper :: MDB m => (String, m()) -> m()
wrapper (name, func) = do
    Log.colorTextT Color.Blue Log.Info "Производится следующая миграция:"
    Log.colorTextT Color.Cyan Log.Info name
    Log.setSettings Color.Cyan False name 
    func
    Log.colorTextT Color.Green Log.Info "Миграция окончена успешно"

pathMigrations :: FilePath
pathMigrations = "migrations/0000_base.sql"

--DB initialization from sql file
base :: MDB m => m ()
base = do 
    queryBS <- File.read pathMigrations
    --Log.dataT Log.Warning queryBS
    let query = Query queryBS
    DB.execute__ query []

--обработка ошибок при запросах к бд!
hashPasswords :: MDB m => m ()
hashPasswords = do
    users <- DB.query_ [sql|SELECT id, login, pass FROM users|]
    Log.textT Log.Info "Получен список пользователей..."
    rows <- DB.executeMany [sql|
        UPDATE users
        SET pass = md5(upd.lp)
        FROM (VALUES (?, ?)) as upd(id, lp)
        WHERE users.id = upd.id
    |] $ map (\(uid, l, p) -> (uid, l <> " " <> p)) (users :: [(Int, B.ByteString, B.ByteString)])
    Log.textT Log.Info "Пароли хешированы..."
    DB.execute_ [sql|
        ALTER TABLE users 
            ALTER COLUMN pass TYPE VARCHAR (32)
    |] []
    Log.textT Log.Info "Урезана длина строки пароля до 32 символов..."

--это тоже можно запихнуть в файл
renameNewsToPosts :: MDB m => m ()
renameNewsToPosts = do
    DB.execute__ [sql|
        DROP TABLE IF EXISTS posts;
        ALTER TABLE news RENAME TO posts;
        ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
        ALTER TABLE comments RENAME COLUMN news_id TO post_id;
    |] []