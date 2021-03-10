 SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id
        WHERE contents.author_id = 1

