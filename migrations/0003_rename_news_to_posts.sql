ALTER TABLE contents RENAME COLUMN news_id TO post_id;
ALTER TABLE comments RENAME COLUMN news_id TO post_id;