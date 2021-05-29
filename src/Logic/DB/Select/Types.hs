{-# LANGUAGE TypeOperators #-}
module Logic.DB.Select.Types where

import Common.Functions
import Data.Map as M ((!))
import Data.Maybe
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types as SQL
import qualified Interface.MCache.Exports as Cache
import qualified Interface.MDB.Exports as DB
import qualified Interface.MError.Exports as Error
import qualified Logic.DB.Row as Row

type Migration =  Row.Migration
type User =  Row.User
type Author = Row.Author :. Row.User
type Category = Row.Category
type Content = Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag :. Maybe Row.Photo
type Draft = Row.Draft :. Content
type Post = Row.Post :. Content
type Tag = Row.Tag
type Comment =  Row.Comment :. Row.Post :. Row.User