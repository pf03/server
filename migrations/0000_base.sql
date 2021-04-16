CREATE TABLE versions (
	id SERIAL PRIMARY KEY,
	name VARCHAR (255) not null
)

CREATE TABLE users (
	id SERIAL PRIMARY KEY,
	first_name VARCHAR (50)  not null,
	last_name VARCHAR (50)  not null,
	avatar VARCHAR (100) not null,
	login VARCHAR (50) unique not null,
	pass VARCHAR (50) not null,
	creation_date DATE  not null,
	is_admin Boolean  not null
);

CREATE TABLE authors (
	id SERIAL PRIMARY KEY,
	user_id INTEGER  not null,
	description VARCHAR (1000)  not null
);

CREATE TABLE categories (
	id SERIAL PRIMARY KEY,
	parent_id INTEGER,
	category_name VARCHAR (1000)  not null
);

CREATE TABLE tags (
	id SERIAL PRIMARY KEY,
	name VARCHAR (50) unique not null 
);

--CONTENT = DRAFT OR NEWS
CREATE TABLE contents (
    id SERIAL PRIMARY KEY,
    author_id INTEGER not null,
    name VARCHAR (50) not null,
    creation_date DATE  not null,
    category_id INTEGER  not null,
    text TEXT  not null,
    photo VARCHAR (100)  not null
);

CREATE TABLE news (
    id SERIAL PRIMARY KEY,
    content_id INTEGER not null
);
CREATE TABLE drafts (
    id SERIAL PRIMARY KEY,
    content_id INTEGER not null,
    news_id INTEGER
);

--many to many
CREATE TABLE tags_to_contents(
    id SERIAL PRIMARY KEY,
    content_id INT  not null,
    tag_id INTEGER  not null
);

CREATE TABLE photos(
    id SERIAL PRIMARY KEY,
    photo VARCHAR (100)  not null,
    content_id INTEGER  not null
);

CREATE TABLE comments (
    id SERIAL PRIMARY KEY,
    news_id INTEGER  not null,
    user_id INTEGER  not null,
    creation_date DATE  not null,
    text text  not null
);



