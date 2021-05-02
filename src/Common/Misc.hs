{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Misc where
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Encode.Pretty         (encodePretty)
import           Data.Aeson.Types
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as L hiding (pack)
import qualified Data.ByteString.Lazy.Char8       as LC
import           Data.Char                        (toLower, toUpper)
import           Data.List.Split
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.String
import           Data.Text                        (Text (..))
import qualified Data.Text                        as T (pack)
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Lazy                   as L hiding (unpack)
import qualified Data.Text.Lazy.Encoding          as L
import           Data.Word                        (Word8)
import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.Types
import           Numeric                          (showHex)

-----------------------------Types---------------------------------------------
type PathInfo = [Text]
type Path = String
data Action = Check | Execute --flag

-----------------------------Template------------------------------------------
-- | Substitution in a template
-- Using: template "Hello {0}, {1}, {0}, {1}," ["Petya", "Vasya"]
class Template s where
    template :: s -> [s] -> s

instance Template BC.ByteString where
  template str args = foldl f str $ zip ts args where
      ts = map (\n -> "{"<> (fromString . show $ n) <>"}") [0,1..]
      f:: BC.ByteString ->  (BC.ByteString,BC.ByteString) -> BC.ByteString
      f acc (t, arg) = replaceB t arg acc where
        replaceB :: BC.ByteString ->  BC.ByteString -> BC.ByteString ->  BC.ByteString
        replaceB t s str = let strs = splitOnB t str in
            mconcat $ init $ foldr (\part acc -> part:s:acc) [] strs
        splitOnB :: BC.ByteString ->  BC.ByteString -> [BC.ByteString]
        splitOnB t str = map BC.pack $ splitOn (BC.unpack t) (BC.unpack str)

instance Template String where
  template str args = foldl f str $ zip ts args where
      ts = map (\n -> ('{':show n)++"}")[0,1..]
      f:: String -> (String, String) -> String
      f acc (t, arg) = replace t arg acc where
        replace :: String -> String -> String -> String
        replace t s str = let strs = splitOn t str in
            concat $ init $ foldr (\part acc -> part:s:acc) [] strs

instance Template Query where
  template (Query str) args = Query $ template str $ map fromQuery args

-----------------------------Convert-------------------------------------------
class Convert a where
    convert :: a -> BC.ByteString

instance Convert BC.ByteString where
  convert = id

-- * EncodeUtf8 for correct Cyrillic encoding
instance Convert String where
    convert = T.encodeUtf8 . T.pack

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

instance Convert Date where
     convert = BC.pack . show

instance Convert Query where
     convert (Query bs) = bs

jc :: Convert a => a -> Maybe BC.ByteString
jc = Just . convert

-----------------------------ConvertL-------------------------------------------
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

-----------------------------Monadic and simple functions----------------------
ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
      Just _ -> m
      _      -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m ()
ifNothing ma m = case ma of
      Nothing -> m
      _       -> return ()

fromJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
fromJustM = forM_

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

readLnT :: MonadIO m => m String
readLnT = liftIO getLine

forMaybe :: [a] -> (a -> Maybe b)  -> [b]
forMaybe = flip mapMaybe

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f list = do
    mb <- forM list f
    return $ catMaybes mb

jlookup :: Eq a => a -> [(a, b)] -> b
jlookup key list = fromJust $ lookup key list

for :: [a] -> (a -> b) -> [b]
for = flip map

(<<$>>) :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
(<<$>>) f mna = do
    na <- mna
    return $ f <$> na
infixl 4 <<$>>

(<$$>) :: Monad m => ([a] -> m b) -> [m a] -> m b
(<$$>) f list = sequenceA list >>= f
infixl 4 <$$>

forMapM :: Monad m => M.Map k v -> (k -> m w) -> m (M.Map k w)
forMapM mp f = sequence $ M.mapWithKey (\k _ -> f k) mp

forMapWithKeyM :: Monad m => M.Map k v -> (k -> v -> m w) -> m (M.Map k w)
forMapWithKeyM mp f = sequence $ M.mapWithKey f mp

adjustM :: Monad m => Ord k => (a -> m a) -> a -> k -> M.Map k a -> m (M.Map k a)
adjustM kl def k  m = do
    let ma = M.lookup k m
    newa <- case ma of 
        Just a -> kl a
        Nothing -> return def
    return $ M.insert k newa m

forMap :: M.Map k v -> (k -> w) -> M.Map k w
forMap mp f = M.mapWithKey (\k _ -> f k) mp

forWithKey :: p -> M.Map k a -> (k -> a -> b) -> M.Map k b
forWithKey f = flip M.mapWithKey

forMMem :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
forMMem cont init f = foldM f init cont

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x  = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x  = init x

_1of3 :: (a,b,c) -> a
_1of3 (a,b,c) = a

_12of3 :: (a,b,c) -> (a,b)
_12of3 (a,b,c) = (a,b)

_2of3 :: (a,b,c) -> b
_2of3 (a,b,c) = b

_3of3 :: (a,b,c) -> c
_3of3 (a,b,c) = c

-----------------------------Translit------------------------------------------
translit :: LC.ByteString -> LC.ByteString
translit = LC.pack . translitStrStr . LC.unpack

translitStr :: String -> LC.ByteString
translitStr = LC.pack . translitStrStr

translitStrStr :: String -> String
translitStrStr = concatMap helper where
    cyr = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
    lat = ["a","b", "v", "g", "d", "e", "yo", "zh", "z", "i", "y", "k", "l", "m", "n", "o", "p", "r", "s", "t",
        "u", "f", "h", "ts", "ch", "sh", "sch", "'", "i", "'", "e", "yu", "ya"]
    dict :: M.Map Char String
    dict = M.fromList $ zip cyr lat
    helper :: Char -> String
    helper c
        | c `elem` cyr = dict M.! c
        | toLower c `elem` cyr = map toUpper $ dict M.! toLower c
        | otherwise = [c]
