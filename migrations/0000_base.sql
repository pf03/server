CREATE TABLE migrations (
	id SERIAL PRIMARY KEY,
	migration_name VARCHAR (255) not null
);

CREATE TABLE users (
	id SERIAL PRIMARY KEY,
	first_name VARCHAR (50)  not null,
	last_name VARCHAR (50)  not null,
	avatar VARCHAR (100) not null,
	user_login VARCHAR (50) unique not null,
	pass VARCHAR (50) not null,
	creation_date DATE not null,
	is_admin Boolean not null
);

CREATE TABLE authors (
	id SERIAL PRIMARY KEY,
	user_id INTEGER not null REFERENCES users (id),
	description VARCHAR (1000) not null
);

CREATE TABLE categories (
	id SERIAL PRIMARY KEY,
	parent_id INTEGER  REFERENCES categories (id),
	category_name VARCHAR (1000) not null
);

CREATE TABLE tags (
	id SERIAL PRIMARY KEY,
	tag_name VARCHAR (50) unique not null 
);

--CONTENT = DRAFT OR NEWS
CREATE TABLE contents (
    id SERIAL PRIMARY KEY,
    author_id INTEGER not null REFERENCES authors (id),
    content_name VARCHAR (50) not null,
    creation_date DATE not null,
    category_id INTEGER not null REFERENCES categories (id),
    content_text TEXT not null,
    main_photo VARCHAR (100)  not null
);

CREATE TABLE news (
    id SERIAL PRIMARY KEY,
    content_id INTEGER not null REFERENCES contents (id)
);
CREATE TABLE drafts (
    id SERIAL PRIMARY KEY,
    content_id INTEGER not null REFERENCES contents (id),
    news_id INTEGER REFERENCES news (id)
);

CREATE TABLE tags_to_contents (
    content_id INTEGER not null REFERENCES contents (id),
    tag_id INTEGER not null REFERENCES tags (id),
    PRIMARY KEY (content_id, tag_id)
);

CREATE TABLE photos (
    id SERIAL PRIMARY KEY,
    photo VARCHAR (100) not null,
    content_id INTEGER not null REFERENCES contents (id)
);

CREATE TABLE comments (
    id SERIAL PRIMARY KEY,
    news_id INTEGER not null REFERENCES news (id),
    user_id INTEGER not null REFERENCES users (id),
    creation_date DATE not null,
    comment_text text not null
);