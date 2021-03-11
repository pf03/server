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