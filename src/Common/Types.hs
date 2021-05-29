{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (Text)

type FileName = String

type PathInfo = [Text]

type Path = String

data Action = Check | Execute --flag

type BS = BC.ByteString

type LBS = LC.ByteString