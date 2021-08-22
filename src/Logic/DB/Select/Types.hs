{-# LANGUAGE TypeOperators #-}

module Logic.DB.Select.Types where

import Database.PostgreSQL.Simple.Types as SQL (type (:.))
import qualified Logic.DB.Row as Row

type Migration = Row.Migration

type User = Row.User

type Author = Row.Author :. Row.User

type Category = Row.Category

type Content = Row.Content :. Row.Category :. Row.Author :. Row.User :. Maybe Row.TagToContent :. Maybe Row.Tag

type Tag = Row.Tag

type Comment = Row.Comment :. Row.Content :. Row.User