ALTER TABLE news RENAME TO posts;
ALTER TABLE drafts RENAME COLUMN news_id TO post_id;
ALTER TABLE comments RENAME COLUMN news_id TO post_id;