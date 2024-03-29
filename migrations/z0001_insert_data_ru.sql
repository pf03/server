INSERT into users (
    last_name,
	first_name,
	avatar,
	login,
	pass,
	creation_date,
	is_admin
) values
    ('DELETED_USER', 'DELETED_USER', 'DELETED_USER.jpg', 'DELETED_USER', 'DELETED_USER', '2000-01-01', False),
	('Ivanov', 'Petr', 'Petr.jpg', 'admin', '123456', '2020-01-01', True),
	('Petrov', 'Ivan', 'Ivan.jpg', 'pivan', 'equalpass', '2000-01-01', False),
	('Yuryev', 'Sergey', 'Sergey.jpg', 'ysergey', 'equalpass', '2015-01-01', False),
	('Pushkin', 'Sergey', 'Pushkin.jpg', 'psergey', 'psergeypass', '1830-01-01', False),
	('Mayakovskiy', 'Vladimir', 'Mayakovskiy.jpg', 'vmayakovskiy', 'vmayakovskiypass', '1920-01-01', False),
	('Москвин', 'Денис', 'Moskvin.jpg', 'dmoskvin', 'dmoskvinpass', '2020-03-03', False);

INSERT into authors (
	user_id,
	description
) values 
    (1, 'DELETED_AUTHOR'),
	(5, 'Алекса́ндр Серге́евич Пу́шкин — русский поэт, драматург и прозаик, заложивший основы русского реалистического направления, критик и теоретик литературы, историк, публицист; один из самых авторитетных литературных деятелей первой трети XIX века.'),
	(6, 'Влади́мир Влади́мирович Маяко́вский — русский советский поэт. Футурист. Один из крупнейших поэтов XX века. Помимо поэзии, ярко проявил себя как драматург, киносценарист, кинорежиссёр, киноактёр, художник, редактор журналов «ЛЕФ», «Новый ЛЕФ»'),
    (7, 'Доцент кафедры математических и информационных технологий, СПбАУ РАН. Один из основателей и координаторов Санкт-Петербургской группы пользователей Haskell (SPbHUG)');

INSERT into categories (
	parent_id,
	category_name
) values 
	(null, 'Языки программирования'),
	(null, 'Поэзия'),
	(1, 'Динамически типизированные ЯП'),
	(3, 'Python'),
	(1, 'Статически типизированные ЯП'),
	(5, 'Haskell'),
	(6, 'Компилятор ghc'),
	(6, 'Монады');

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

INSERT INTO contents (
    author_id,
    name,
    creation_date,
    category_id,
    text,
    photo
)
VALUES
    (
        4,
        'GHC 8.10.1',
        '2020-03-20',
        6,
        'Состоялся очередной релиз компилятора Glasgow Haskell Compiler.',
        'logo.jpg'
    ),(
        2,
        'Я помню чудное мгновенье, черновик',
        '1825-03-20',
        2,
        'Черновик: Я помню чудное мгновенье:\nПередо мной явилась ты,\nКак мимолетное виденье,\nКак гений чистой красоты.',
        'pushkin.jpg'
    ),(
        2,
        'Я помню чудное мгновенье',
        '1825-03-20',
        2,
        'Я помню чудное мгновенье:\nПередо мной явилась ты,\nКак мимолетное виденье,\nКак гений чистой красоты.',
        'pushkin.jpg'
    );

INSERT INTO news (content_id) VALUES (1), (3);

INSERT INTO drafts (content_id, news_id) VALUES (2, null);

INSERT INTO tags_to_contents (content_id, tag_id) VALUES
    (1,1),(1,2),(1,5),(1,6),(1,9),(2,3),(3,3);

INSERT INTO photos (photo, content_id) VALUES
    ('haskell.jpg', 1),
    ('open_source.jpg', 1),
    ('poem.jpg', 2);

INSERT INTO comments (news_id, user_id, creation_date, text) VALUES
    (1, 2, '2020-03-21', 'Какая замечательная новость...'),
    (1, 3, '2020-03-22', 'Какая превосходная новость...'),
    (1, 4, '2020-03-23', 'Какая великолепная новость...')