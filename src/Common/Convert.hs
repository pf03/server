module Common.Convert where

import Common.Types (BS, LBS)
import Data.Aeson (Object, Value, encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L hiding (unpack)
import qualified Data.Text.Lazy.Encoding as L
import Database.PostgreSQL.Simple.Time (Date)
import Database.PostgreSQL.Simple.Types (Query (Query))

-----------------------------Convert-------------------------------------------
class Convert a where
  convert :: a -> BS

instance Convert BS where
  convert = id

-- * EncodeUtf8 for correct Cyrillic encoding

instance Convert String where
  convert = T.encodeUtf8 . T.pack

instance Convert LBS where
  convert = BC.pack . LC.unpack

instance Convert Int where
  convert = BC.pack . show

instance Convert Float where
  convert = BC.pack . show

instance Convert Value where
  convert = convert . encode

instance Convert Object where
  convert = convert . encode

instance Convert Date where
  convert = BC.pack . show

instance Convert Query where
  convert (Query bs) = bs

jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert

-----------------------------ConvertL-------------------------------------------
class ConvertL a where
  convertL :: a -> LBS

instance ConvertL String where
  convertL = L.encodeUtf8 . L.pack -- encodeUtf8 for correct cyrillic encoding

instance ConvertL BS where
  convertL = LC.pack . BC.unpack

instance ConvertL LBS where
  convertL = id

instance ConvertL Int where
  convertL = LC.pack . show

instance ConvertL Float where
  convertL = LC.pack . show

instance ConvertL Value where
  convertL = convertL . encode

instance ConvertL Object where
  convertL = convertL . encode
