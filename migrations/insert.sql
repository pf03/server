 SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        WHERE contents.author_id = 1;


SELECT COUNT(posts.id) FROM posts
            LEFT JOIN contents ON contents.id = posts.content_id
            WHERE contents.author_id = 4;

SELECT * FROM posts LEFT JOIN contents ON
contents.id = posts.content_id
    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
WHERE TRUE AND (TRUE OR TRUE
OR TRUE OR TRUE OR TRUE) AND TRUE AND TRUE
AND TRUE AND TRUE AND TRUE LIMIT 20 OFFSET
0;

SELECT * FROM comments
    LEFT JOIN posts ON posts.id = comments.post_id
    LEFT JOIN users ON users.id = comments.user_id
WHERE posts.id = 1 LIMIT 20 OFFSET 0

INSERT into comments (post_id, user_id, creation_date, text)
    values (1, 1, current_date, 'text')

INSERT into users (last_name, first_name,
avatar, login, pass, creation_date, is_admin) values
('last_name' , 'first_name' , 'avatar' , 'login' , md5 ('pass') , current_date , FALSE)

<<<<<<< HEAD
SELECT * FROM posts LEFT JOIN contents ON contents.id = posts.content_id

    LEFT JOIN categories ON categories.id = contents.category_id
    LEFT JOIN authors ON authors.id = contents.author_id
    LEFT JOIN users ON users.id = authors.user_id
    LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
    LEFT JOIN tags ON tags.id = tags_to_contents.tag_id WHERE TRUE AND (TRUE OR TRUE OR TRUE OR TRUE OR TRUE) AND TRUE AND
TRUE AND TRUE AND TRUE AND TRUE LIMIT 20 OFFSET 0
=======
SELECT DISTINCT *
FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
    WHERE tags.id>3;

--первый запрос для нахождения подходящих posts.id с фильтрами
SELECT DISTINCT posts.id
              --, contents, categories, authors, users, tags
FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
        LEFT JOIN tags_to_contents ON contents.id = tags_to_contents.content_id
        LEFT JOIN tags ON tags.id = tags_to_contents.tag_id
    WHERE tags.id>3; -- ...

--второй запрос для нахождения сущностей, уже без фильтров
SELECT *
FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        LEFT JOIN categories ON categories.id = contents.category_id
        LEFT JOIN authors ON authors.id = contents.author_id
        LEFT JOIN users ON users.id = authors.user_id
    WHERE posts.id IN (1,2)
>>>>>>> 965a6632ac4e7e4eeebc74ef4ad97bf6dbdb781c
