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
list :: M.Map String (T())
list = M.fromList [
    ("УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными", base),
    ("Хеширование паролей", hashPasswords),
    ("Переименование news в posts", renameNewsToPosts)
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
        "Y" -> do
            base
            hashPasswords
            renameNewsToPosts
        _ -> do
            putStrLnT "Неверный выбор. Попробуйте снова"
            Migrations.all


pathMigrations :: FilePath
pathMigrations = "migrationldfffffffffs/0000_base.sql"

--DB initialization from sql file
base :: T()
base = do 
    Log.textT Log.Warning "Производится следующая миграция: УДАЛЕНИЕ таблиц, создание новых и наполнение тестовыми данными"
    queryBS <- toT $ Transformer.readFile pathMigrations
    let query = Query queryBS
    execute_ query
    Log.textT Log.Info "Миграция окончена успешно"


--обработка ошибок при запросах к бд!
hashPasswords :: T()
hashPasswords = do
    Log.setSettings Color.Blue True "application" 
    Log.textT Log.Warning "Производится следующая миграция: хеширование паролей"
    -- conn <- S.getConnection
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
    |]
    Log.textT Log.Info "Урезана длина строки пароля до 32 символов..."
    Log.textT Log.Info "Миграция окончена успешно"

renameNewsToPosts :: T()
renameNewsToPosts = do
    Log.setSettings Color.Blue True "application" 
    Log.textT Log.Warning "Производится следующая миграция: переименование news в posts"
    execute_ [sql|
        DROP TABLE IF EXISTS posts;
        ALTER TABLE news RENAME TO posts;
        ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
        ALTER TABLE comments RENAME COLUMN news_id TO post_id;
    |]
    Log.textT Log.Info "Миграция окончена успешно"




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
