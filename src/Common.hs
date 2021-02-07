{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Common where
import Control.Monad.IO.Class
import Data.List.Split

--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Aeson.Types
import Data.Aeson
import Data.Text.Encoding
import Data.Text (pack)


----------------вспомогательные монадические функции -------------------------------------
ifJust :: Monad m => Maybe a -> m () -> m () 
ifJust ma m = case ma of 
      Just _ -> m
      _ -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m () 
ifNothing ma m = case ma of 
      Nothing -> m
      _ -> return ()

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

----------------для работы со строками------------------------------------------------------
--подстановка в шаблон
template :: String -> [String] -> String
template str args = foldl f str $ zip ts args where
    ts = map (\n -> ('{':show n)++"}")[0,1..]
    f:: String -> (String, String) -> String
    f acc (t, arg) = replace t arg acc

--замена одной строки на другую
replace :: String -> String -> String -> String 
replace t s str = let strs = splitOn t str in
    concat $ init $ foldr (\part acc -> part:s:acc) [] strs 

-- test = template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
-- t1 = replace "{0}" "Vasya" "Hello {0}, {1}, {0}, {1}," 

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

----------------------------------------Convert--------------------------------------------------
class Convert a where
    convert :: a -> BC.ByteString

instance Convert String where
    convert = encodeUtf8 . pack  --encodeUtf8 для корректной кодировки кирилицы

instance Convert LC.ByteString where
  convert = BC.pack . LC.unpack

instance Convert Int where
  convert = BC.pack . show

instance Convert Float where
  convert = BC.pack . show

instance Convert Value where 
     convert = convert . encode 

instance Convert Object where 
     convert = convert . encode 

jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert