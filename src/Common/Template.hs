module Common.Template where

import Common.Types (BS)
import qualified Data.ByteString.Char8 as BC
import Data.List.Split (splitOn)
import Data.String (IsString (fromString))
import Database.PostgreSQL.Simple.Types (Query (..))

-- | Substitution in a template
-- Using: template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
class Template s where
  template :: s -> [s] -> s

instance Template BS where
  template str args = foldl f str $ zip ts args
    where
      ts = map (\n -> "{" <> (fromString . show $ n) <> "}") [0 :: Int, 1 ..]
      f :: BS -> (BS, BS) -> BS
      f acc (t, arg) = replaceB t arg acc
        where
          replaceB :: BS -> BS -> BS -> BS
          replaceB t0 s str0 =
            let strs = splitOnB t0 str0
             in mconcat $ init $ foldr (\part acc0 -> part : s : acc0) [] strs
          splitOnB :: BS -> BS -> [BS]
          splitOnB t1 str1 = map BC.pack $ splitOn (BC.unpack t1) (BC.unpack str1)

instance Template String where
  template str args = foldl f str $ zip ts args
    where
      ts = map (\n -> ('{' : show n) ++ "}") [0 :: Int, 1 ..]
      f :: String -> (String, String) -> String
      f acc (t, arg) = replace t arg acc
        where
          replace :: String -> String -> String -> String
          replace t0 s str0 =
            let strs = splitOn t0 str0
             in concat $ init $ foldr (\part acc0 -> part : s : acc0) [] strs

instance Template Query where
  template (Query str) args = Query $ template str $ map fromQuery args

templateM :: (Template s, Monad m) => s -> [m s] -> m s
templateM str mArgs = do
  args <- sequenceA mArgs
  return $ template str args