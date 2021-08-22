INSERT into users (
    last_name,
	first_name,
	avatar,
	user_login,
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
	('Moskvin', 'Denis', 'Moskvin.jpg', 'dmoskvin', 'dmoskvinpass', '2020-03-03', False);

INSERT into authors (
	user_id,
	description
) values 
    (1, 'DELETED_AUTHOR'),
	(5, 'Aleksandr Sergeeevich Púshkin — russkij poet, dramaturg i prozaik, zalozhivshij osnovy russkogo realisticheskogo napravleniya, kritik i teoretik literatury, istorik, publicist; odin iz samyh avtoritetnyh literaturnyh deyatelej pervoj treti XIX veka.'),
	(6, 'Vladiimir Vladimirovich Mayakovskij — russkij sovetskij poet. Futurist. Odin iz krupnejshih poetov XX veka. Pomimo poezii, yarko proyavil sebya kak dramaturg, kinoscenarist, kinorezhissyor, kinoaktyor, hudozhnik, redaktor zhurnalov «LEF», «Novyj LEF»'),
    (7, 'Docent kafedry matematicheskih i informacionnyh tekhnologij, SPbAU RAN. Odin iz osnovatelej i koordinatorov Sankt-Peterburgskoj gruppy polzovatelej Haskell (SPbHUG)');

INSERT into categories (
	parent_id,
	category_name
) values 
	(null, 'Yazyki programmirovaniya'),
	(null, 'Poeziya'),
	(1, 'Dinamicheski tipizirovannye YAP'),
	(3, 'Python'),
	(1, 'Staticheski tipizirovannye YAP'),
	(5, 'Haskell'),
	(6, 'Kompilyator ghc'),
	(6, 'Monady');

INSERT into tags (
	tag_name
) values
	('yazyk_programmirovaniya'),
	('programmirovanie'),
	('pushkin'),
	('esenin'),
	('moskvin'),
	('haskell'),
	('python'),
	('monad'),
	('ghc');

INSERT INTO contents (
    author_id,
    content_name,
    creation_date,
    category_id,
    content_text,
    main_photo,
    photos,
    is_draft,
    news_id
)
VALUES
    (
        4,
        'GHC 8.10.1',
        '2020-03-20',
        6,
        'Sostoyalsya ocherednoj reliz kompilyatora Glasgow Haskell Compiler.',
        'logo.jpg',
        '{"haskell.jpg", "open_source.jpg"}',
        FALSE,
        null
    ),(
        2,
        'Ya pomnyu chudnoe mgnovenye, chernovik',
        '1825-03-20',
        2,
        'CHernovik: Ya pomnyu chudnoe mgnovenye:\nPeredo mnoj yavilas ty,\nKak mimoletnoe videnye,\nKak genij chistoj krasoty.',
        'pushkin.jpg',
        '{"poem.jpg"}',
        TRUE,
        null
    ),(
        2,
        'Ya pomnyu chudnoe mgnovenye',
        '1825-03-20',
        2,
        'Ya pomnyu chudnoe mgnovenye:\nPeredo mnoj yavilas ty,\nKak mimoletnoe videnye,\nKak genij chistoj krasoty.',
        'pushkin.jpg',
       '{"poem.jpg"}',
        FALSE,
        null
    );

INSERT INTO tags_to_contents (content_id, tag_id) VALUES
    (1,1),(1,2),(1,5),(1,6),(1,9),(2,3),(3,3);

INSERT INTO comments (news_id, user_id, creation_date, comment_text) VALUES
    (1, 2, '2020-03-21', 'Kakaya zamechatelnaya novost...'),
    (1, 3, '2020-03-22', 'Kakaya prevoskhodnaya novost...'),
    (1, 4, '2020-03-23', 'Kakaya velikolepnaya novost...')
