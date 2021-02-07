module Migrations where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
-- import Crypto.Hash.MD5
import  qualified Data.ByteString as B
import Types



hashPasswords :: T()
hashPasswords = do
    return()

_hashPasswords :: IO()
_hashPasswords = do
    --это в модуль DB!!
    -- разобраться с пакетом postgresql-simple-migration
    let connectDBInfo  = ConnectInfo {connectHost = "127.0.0.1", connectPort = 5432, connectUser = "postgres", connectPassword = "demo", connectDatabase = "server"}
    conn <- connect connectDBInfo
    putStrLn "Производится следующая миграция: хеширование паролей"
    let num = 3
    users <- query_ conn [sql|SELECT id, login, pass FROM users|] :: IO [(Int, B.ByteString, B.ByteString)]
    putStrLn "Получен список пользователей..."
    -- rows <- execute_ conn [sql|
    --     ALTER TABLE users 
    --         ALTER COLUMN pass TYPE bytea USING pass::bytea
    -- |]
    -- putStrLn $ "rows affected: " ++ show rows

    -- executeMany conn [sql|
    --     INSERT INTO  VALUES (?,?)
    -- |] ([(1, "hello"),(2, "world")]::[(Int, String)])

    -- print users
    -- let hashed_passwords = map (\(uid, pass) -> (uid, hash pass)) passwords
    --let hashed_passwords = map (\(uid, pass) -> (uid, "haskellpassword"::B.ByteString)) passwords
    -- return()


--можно хешировать login ++ " " ++ pass для того, чтобы одинаковые пароли имели разный хеш
--в общем случае возможны коллизии
    rows <- executeMany conn [sql|
        UPDATE users
        SET pass = md5(upd.lp)
        FROM (VALUES (?, ?)) as upd(id, lp)
        WHERE users.id = upd.id
    |] $ map (\(uid, l, p) -> (uid, l <> " " <> p)) users
    putStrLn "Пароли хешированы..."

    -- putStrLn  $ "affected "++show rows ++" rows" 
    
    execute_ conn [sql|
        ALTER TABLE users 
            ALTER COLUMN pass TYPE VARCHAR (32)
    |]
    putStrLn "Урезана длина строки пароля до 32 символов..."
    putStrLn "Миграция окончена успешно"
    return()
