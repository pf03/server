--Хеширование паролей
UPDATE users SET pass = md5 (CONCAT_WS(' ', login, pass))

--в каждую миграцию можно добавить информацию о самой миграции
--тогда коду не нужна никакая дополнительная информация, кроме этого файла

-- INSERT INTO migrations (name, description) VALUES ({0}, "Хеширование паролей");


-- INSERT into migrations (name) values ('0000_base.sql')