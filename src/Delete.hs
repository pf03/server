{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Delete where

import Database.PostgreSQL.Simple.FromRow --hiding (FromRow(..) ) 
import Database.PostgreSQL.Simple.Time
import GHC.Generics 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, Text(..))
import Types 
import qualified Row
import Database.PostgreSQL.Simple.Types as SQL
import Database.PostgreSQL.Simple.SqlQQ
import Common
import Query
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Identity
import Select
import Data.Map as M ((!), fromList)
import Class
import Control.Monad.Trans.Except
import Transformer
import qualified Log
import Data.Maybe
import Data.Monoid
import Data.Int

data Changed = Changed {
    -- created :: Created,
    -- edited :: Edited,
    deleted :: Deleted
    } deriving (Show, Generic)

instance Semigroup Changed where 
    (<>) = mappend

instance Monoid Changed where
    mempty = Changed mempty
    mappend (Changed c1) (Changed c2) = Changed (mappend c1 c2)

instance ToJSON Changed

data Deleted = Deleted {
    users :: Int64,
    authors :: Int64,
    cetegories :: Int64,
    tags :: Int64,
    posts :: Int64,
    drafts :: Int64,
    photos :: Int64,
    comments :: Int64
} deriving (Show, Generic)

instance ToJSON Deleted
instance Semigroup Deleted where 
    (<>) = mappend

instance Monoid Deleted where
    mempty = Deleted 0 0 0 0 0 0 0 0
    mappend (Deleted a1 b1 c1 d1 e1 f1 g1 h1) (Deleted a2 b2 c2 d2 e2 f2 g2 h2) = Deleted (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2) (f1 + f2) (g1 + g2) (h1 + h2)


user :: Int -> T Changed
user pid = do 
    --проверка на связанные сущности
    checkNotExist pid "автора" "новости" $ template [sql|
        SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id   
        WHERE contents.author_id = {0}
    |] [q pid]   
    authors <- execute_ $ template [sql|DELETE FROM users WHERE id = {0}|] [q pid]
    return $ mempty {Delete.deleted = mempty {Delete.authors = authors}}

--юзеров и авторов на практике удаляют часто, поэтому весь контент ими созданный нужно переписать на удаленного юзера. Или же самого юзера пометить в базе данных как удаленного
--тогда в других апи нужно это обрабатывать
author :: Int -> T Changed
author pid = do 
    --проверка на связанные сущности
    checkNotExist pid "автора" "новости" $ template [sql|
        SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id   
        WHERE contents.author_id = {0}
    |] [q pid]   
    authors <- execute_ $ template [sql|DELETE FROM authors WHERE id = {0}|] [q pid]
    return $ mempty {Delete.deleted = mempty {Delete.authors = authors}}


--для поста каскадное удаление
post :: Int -> T Changed 
post pid = do 
    --проверка на связанные сущности
    checkNotExist pid "пост" "черновик" $ template [sql|
        SELECT posts.id, contents.name FROM posts
        LEFT JOIN contents ON contents.id = posts.content_id   
        WHERE contents.author_id = {0}
    |] [q pid]   
    [Only contentId] <- query_ $ template [sql|SELECT posts.content_id FROM posts WHERE id = {0}|] [q pid] :: T[Only Int] 
    posts <- execute_ $ template [sql|DELETE FROM posts WHERE id = {0}|] [q pid]   
    execute__ $ template [sql|DELETE FROM contents WHERE id = {0}|] [q contentId]   
    drafts <- execute_ $ template [sql|DELETE FROM drafts WHERE post_id = {0}|] [q pid] 
    execute__ $ template [sql|DELETE FROM tags_to_contents WHERE content_id = {0}|] [q contentId]   
    photos <- execute_ $ template [sql|DELETE FROM photos WHERE content_id = {0}|] [q contentId]   
    comments <- execute_ $ template [sql|DELETE FROM comments WHERE post_id = {0}|] [q pid]
    return $ mempty {Delete.deleted = mempty {
            Delete.posts = posts,
            Delete.drafts = drafts,
            Delete.photos = photos,
            Delete.comments = comments
        }
    }




checkNotExist :: Int -> String -> String -> Query -> T() 
checkNotExist pid name1 name2 templ = do
    results <- query_ $ template templ [q pid] :: T [(Int, String)]
    case results of
        [] -> return ()
        _ -> throwT $ DBError  (template "Невозможно удалить {0}, так как к нему привязаны следующие {1}:\n{2}" [name1, name2, showResults]) where
            showResults = concatMap helper results
            helper :: (Int, String) -> String 
            helper (pid2, name2) = template "id = {0}, name = {1}\n" [show pid2, name2]


-- strdddd = "CREATE TABLE IF NOT EXISTS mails ( \
--             \    id TEXT PRIMARY KEY NOT NULL, \
--             \    \"from\" TEXT NOT NULL, \
--             \    \"to\" TEXT NOT NULL, \
--             \    subject TEXT NOT NULL, \
--             \    source TEXT NOT NULL \
--             \)"

        -- SELECT (posts.id, contents.name) FROM posts  --в скобках row
        -- LEFT JOIN contents ON contents.id = posts.content_id   
        -- WHERE contents.author_id = {0}