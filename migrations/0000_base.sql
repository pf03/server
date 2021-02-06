
DROP TABLE IF EXISTS users, authors, cathegories, tags, news, tags_to_news;

CREATE TABLE users (
	id SERIAL PRIMARY KEY,
	first_name VARCHAR (50),
	last_name VARCHAR (50),
	avatar VARCHAR (100),
	login VARCHAR (100) unique not null,
	pass VARCHAR (100),
	creation_date DATE,
	is_admin Boolean
);

INSERT into users (
	first_name,
	last_name,
	avatar,
	login,
	pass,
	creation_date,
	is_admin
) values 
	('Ivanov', 'Petr', 'Petr.jpg', 'admin', '123456', '2020-01-01', True),
	('Petrov', 'Ivan', 'Ivan.jpg', 'pivan', 'qwerty', '2000-01-01', False),
	('Yuryev', 'Sergey', 'Sergey.jpg', 'ysergey', '1234', '2015-01-01', False),
	('Pushkin', 'Sergey', 'Pushkin.jpg', 'psergey', 'psergeypass', '1830-01-01', False),
	('Mayakovskiy', 'Vladimir', 'Mayakovskiy.jpg', 'vmayakovskiy', 'vmayakovskiypass', '1920-01-01', False),
	('Moskvin', 'Denis', 'Moskvin.jpg', 'dmoskvin', 'dmoskvinpass', '2020-03-03', False);
	
--пароли нужно закодировать
--это будет первая миграция

CREATE TABLE authors (
	id SERIAL PRIMARY KEY,
	user_id INTEGER,
	description VARCHAR (1000)
);

INSERT into authors (
	user_id,
	description
) values 
	(4, 'Алекса́ндр Серге́евич Пу́шкин — русский поэт, драматург и прозаик, заложивший основы русского реалистического направления, критик и теоретик литературы, историк, публицист; один из самых авторитетных литературных деятелей первой трети XIX века.'),
	(5, 'Влади́мир Влади́мирович Маяко́вский — русский советский поэт. Футурист. Один из крупнейших поэтов XX века. Помимо поэзии, ярко проявил себя как драматург, киносценарист, кинорежиссёр, киноактёр, художник, редактор журналов «ЛЕФ», «Новый ЛЕФ»'),
    (6, 'Доцент кафедры математических и информационных технологий, СПбАУ РАН. Один из основателей и координаторов Санкт-Петербургской группы пользователей Haskell (SPbHUG)');

CREATE TABLE cathegories (
	id SERIAL PRIMARY KEY,
	parent_id INTEGER,
	cathegory_name VARCHAR (1000)
);

INSERT into cathegories (
	parent_id,
	cathegory_name
) values 
	(null, 'Языки программирования'),
	(null, 'Поэзия'),
	(1, 'Динамически типизированные ЯП'),
	(3, 'Python'),
	(1, 'Статически типизированные ЯП'),
	(5, 'Haskell'),
	(6, 'Компилятор ghc'),
	(6, 'Монады');

CREATE TABLE tags (
	id SERIAL PRIMARY KEY,
	name VARCHAR (50) not null
);

INSERT into tags (
	name
) values
	('язык_программирования'),
	('программирование'),
	('пушкин'),
	('есенин'),
	('москвин'),
	('haskell'),
	('python'),
	('monad'),
	('ghc');

CREATE TABLE news (
    id SERIAL PRIMARY KEY,
    name VARCHAR (50) not null,
    creation_date DATE,
    autor_id INTEGER,
    cathegory_id INTEGER,
    text TEXT,
    photo VARCHAR (100)
);
INSERT INTO news (
    name,
    creation_date,
    autor_id,
    cathegory_id,
    text,
    photo
)
VALUES
    ('GHC 8.10.1',
     '2020-03-20',
     6,
     6,
     'Состоялся очередной релиз компилятора Glasgow Haskell Compiler.',
     'logo.jpg'
);
CREATE TABLE tags_to_news(
    id SERIAL PRIMARY KEY,
    news_id INTEGER,
    tag_id INT
);
INSERT INTO tags_to_news (news_id, tag_id) VALUES
    (1,1),(1,2),(1,5),(1,6),(1,9)

CREATE TABLE photo_to_news(
    id SERIAL PRIMARY KEY,
    news_id INTEGER,
    photo VARCHAR (100)
);
INSERT INTO photo_to_news (news_id, photo) VALUES
    (1,'haskell.jpg'),(1,'open_source.jpg')