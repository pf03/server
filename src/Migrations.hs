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
import Class
import qualified State as S
import qualified Data.ByteString.Lazy as L
import Control.Exception
import Data.Int
import Control.Monad
import Query
import Common
import qualified Data.Map as M
import Transformer

--использовать этот список
--list :: M.Map String (T())
list = --M.fromList 
    [
        ("0000 УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными", base),
        ("0001 Хеширование паролей", hashPasswords),
        ("0002 Переименование news в posts", renameNewsToPosts)
    ]


--сделать возможность выбора номера миграции
all :: T()
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

allForce :: T()
allForce = do
    Log.colorTextT Color.Blue Log.Info "Производятся миграции..."
    Log.off
    mapM_ Migrations.wrapper Migrations.list
    Log.on
    Log.colorTextT Color.Green Log.Info "Все миграции выполнены успешно."

wrapper :: (String, T()) -> T()
wrapper (name, func) = do
    Log.colorTextT Color.Blue Log.Info "Производится следующая миграция:"
    Log.colorTextT Color.Cyan Log.Info name
    Log.setSettings Color.Cyan False name 
    func
    Log.colorTextT Color.Green Log.Info "Миграция окончена успешно"

pathMigrations :: FilePath
pathMigrations = "migrations/0000_base.sql"

--DB initialization from sql file
base :: T()
base = do 
    queryBS <- toT $ Transformer.readFile pathMigrations
    --Log.dataT Log.Warning queryBS
    let query = Query queryBS
    execute__ query []

--обработка ошибок при запросах к бд!
hashPasswords :: T()
hashPasswords = do
    users <- query_ [sql|SELECT id, login, pass FROM users|] :: T [(Int, B.ByteString, B.ByteString)]
    Log.textT Log.Info "Получен список пользователей..."
    rows <- executeMany [sql|
        UPDATE users
        SET pass = md5(upd.lp)
        FROM (VALUES (?, ?)) as upd(id, lp)
        WHERE users.id = upd.id
    |] $ map (\(uid, l, p) -> (uid, l <> " " <> p)) users
    Log.textT Log.Info "Пароли хешированы..."
    execute_ [sql|
        ALTER TABLE users 
            ALTER COLUMN pass TYPE VARCHAR (32)
    |] []
    Log.textT Log.Info "Урезана длина строки пароля до 32 символов..."

--это тоже можно запихнуть в файл
renameNewsToPosts :: T()
renameNewsToPosts = do
    execute__ [sql|
        DROP TABLE IF EXISTS posts;
        ALTER TABLE news RENAME TO posts;
        ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
        ALTER TABLE comments RENAME COLUMN news_id TO post_id;
    |] []

--Было
-- CREATE TABLE posts (
--     id SERIAL PRIMARY KEY,
--     content_id INTEGER not null
-- );
-- CREATE TABLE drafts (
--     id SERIAL PRIMARY KEY,
--     content_id INTEGER not null,
--     posts_id INTEGER
-- );
--стало
-- CREATE TABLE posts (
--     id SERIAL PRIMARY KEY,
--     content_id INTEGER not null
--     draft_id INTEGER not null
-- );
-- CREATE TABLE drafts (
--     id SERIAL PRIMARY KEY,
--     content_id INTEGER not null,
-- );
--они могут ссылаться на разный контент, так как черновик возможно отредактировали, но не опубликовали



-- testException :: IO()
-- testException = do
--     let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
--     conn <- connect connectDBInfo
--     (execute_ conn [sql| WRONG SQL |]) `catch` sqlhandler


--     return()

-- sqlhandler :: SqlError -> IO Int64
-- sqlhandler e = do
--     putStrLn "Неверный запрос"
--     return 0





-- _hashPasswords :: IO()
-- _hashPasswords = do
--     --это в модуль DB!!
--     -- разобраться с пакетом postgresql-simple-migration
--     let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
--     conn <- connect connectDBInfo
--     putStrLn "Производится следующая миграция: хеширование паролей"
--     let num = 3
--     users <- query_ conn [sql|SELECT id, login, pass FROM users|] :: IO [(Int, B.ByteString, B.ByteString)]
--     putStrLn "Получен список пользователей..."
--     -- rows <- execute_ conn [sql|
--     --     ALTER TABLE users 
--     --         ALTER COLUMN pass TYPE bytea USING pass::bytea
--     -- |]
--     -- putStrLn $ "rows affected: " ++ show rows

--     -- executeMany conn [sql|
--     --     INSERT INTO  VALUES (?,?)
--     -- |] ([(1, "hello"),(2, "world")]::[(Int, String)])

--     -- print users
--     -- let hashed_passwords = map (\(uid, pass) -> (uid, hash pass)) passwords
--     --let hashed_passwords = map (\(uid, pass) -> (uid, "haskellpassword"::B.ByteString)) passwords
--     -- return()


-- --можно хешировать login ++ " " ++ pass для того, чтобы одинаковые пароли имели разный хеш
-- --в общем случае возможны коллизии
--     rows <- executeMany conn [sql|
--         UPDATE users
--         SET pass = md5(upd.lp)
--         FROM (VALUES (?, ?)) as upd(id, lp)
--         WHERE users.id = upd.id
--     |] $ map (\(uid, l, p) -> (uid, l <> " " <> p)) users
--     putStrLn "Пароли хешированы..."

--     -- putStrLn  $ "affected "++show rows ++" rows" 
    
--     execute_ conn [sql|
--         ALTER TABLE users 
--             ALTER COLUMN pass TYPE VARCHAR (32)
--     |]
--     putStrLn "Урезана длина строки пароля до 32 символов..."
--     putStrLn "Миграция окончена успешно"
--     return()
