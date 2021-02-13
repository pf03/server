-- WITH passwords AS (
--     SELECT FROM products
--     WHERE
--         "date" >= '2010-10-01' AND
--         "date" < '2010-11-01'
--     RETURNING *
-- )
-- INSERT INTO products_log
-- SELECT * FROM moved_rows;




-- UPDATE users SET pass = digest($1, 'sha1')

-- UPDATE users
--         SET pass = upd.pass
--         FROM (VALUES (1,'newpass')) as upd(id,pass)
--         WHERE users.id = upd.id

-- ALTER TABLE users
--     ALTER COLUMN pass TYPE bytea USING pass::bytea;

-- select e.extname, n.nspname from pg_catalog.pg_extension e left join pg_catalog.pg_namespace n on n.oid = e.extnamespace;

-- CREATE OR REPLACE FUNCTION sha1(bytea) returns text AS $$
--       SELECT encode(digest($1, 'sha1'), 'hex')
--     $$ LANGUAGE SQL STRICT IMMUTABLE;

-- UPDATE users
--         SET pass = md5(upd.pass)
--         FROM (VALUES (1, 'admin')) as upd(id,pass)
--         WHERE users.id = upd.id

DROP TABLE IF EXISTS posts;
ALTER TABLE news RENAME TO posts;
ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
ALTER TABLE comments RENAME COLUMN news_id TO post_id;
