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
import qualified Data.Text.Encoding as B
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Text as B (pack)
import Database.PostgreSQL.Simple.Types

import qualified Data.Text.Lazy as L
import Data.String


----------------вспомогательные монадические функции -------------------------------------
ifJust :: Monad m => Maybe a -> m () -> m () 
ifJust ma m = case ma of 
      Just _ -> m
      _ -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m () 
ifNothing ma m = case ma of 
      Nothing -> m
      _ -> return ()

fromJustM :: Monad m => Maybe a -> (a -> m ()) -> m () 
fromJustM ma mf = case ma of 
      Just a -> mf a
      _ -> return ()

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

readLnT :: MonadIO m => m String
readLnT = liftIO getLine

----------------для работы со строками------------------------------------------------------
--подстановка в шаблон

class Template s where
    template :: s -> [s] -> s


instance Template BC.ByteString where 
  --template :: BC.ByteString ->  [BC.ByteString] -> BC.ByteString
  template str args = foldl f str $ zip ts args where
      ts = map (\n -> "{"<> (fromString . show $ n) <>"}") [0,1..]
      f:: BC.ByteString ->  (BC.ByteString,BC.ByteString) -> BC.ByteString
      f acc (t, arg) = replaceB t arg acc where
        --замена одной строки на другую
        replaceB :: BC.ByteString ->  BC.ByteString -> BC.ByteString ->  BC.ByteString
        replaceB t s str = let strs = splitOnB t str in
            mconcat $ init $ foldr (\part acc -> part:s:acc) [] strs 
        --не очень эффективное решение!!!!!!!!!!
        splitOnB :: BC.ByteString ->  BC.ByteString -> [BC.ByteString]
        splitOnB t str = map BC.pack $ splitOn (BC.unpack t) (BC.unpack str) 

instance Template String where 
  --template :: String -> [String] -> String
  template str args = foldl f str $ zip ts args where
      ts = map (\n -> ('{':show n)++"}")[0,1..]
      f:: String -> (String, String) -> String
      f acc (t, arg) = replace t arg acc where
        --замена одной строки на другую
        replace :: String -> String -> String -> String 
        replace t s str = let strs = splitOn t str in
            concat $ init $ foldr (\part acc -> part:s:acc) [] strs 

instance Template Query where 
  template (Query str) args = Query $ template str $ map fromQuery args 


-- test = template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
-- t1 = replace "{0}" "Vasya" "Hello {0}, {1}, {0}, {1}," 

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

-- splitOnB :: [BC.ByteString] -> BC.ByteString ->  BC.ByteString -> [BC.ByteString]
-- splitOnB acc template str | beginFromTemplateB template str  = splitOnB (acc++[mempty]) template $ BC.drop (BC.length template) str
-- splitOnB (a:as) template str = splitOnB ((a <> (BC.head str)):as ) template (BC.tail str)
-- --splitOnB template str | template /= str == []:splitOnB template str



-- beginFromTemplateB :: BC.ByteString ->  BC.ByteString -> Bool
-- beginFromTemplateB template str = if length str < length template then False else take (length template) str == template

----------------------------------------Convert--------------------------------------------------
--сделать двухпараметрический класс типов
--strict version
class Convert a where
    convert :: a -> BC.ByteString

instance Convert BC.ByteString where
  convert = id

instance Convert String where
    convert = B.encodeUtf8 . B.pack  --encodeUtf8 для корректной кодировки кирилицы

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

--lazy version
class ConvertL a where
    convertL :: a -> LC.ByteString

instance ConvertL String where
    convertL = L.encodeUtf8 . L.pack  --encodeUtf8 для корректной кодировки кирилицы

instance ConvertL BC.ByteString where
  convertL = LC.pack . BC.unpack

instance ConvertL LC.ByteString where
  convertL = id

instance ConvertL Int where
  convertL = LC.pack . show

instance ConvertL Float where
  convertL = LC.pack . show

instance ConvertL Value where 
     convertL = convertL . encode 

instance ConvertL Object where 
     convertL = convertL . encode 




jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert


