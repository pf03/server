{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Functions where

import Common.Types (BS)
import Control.Monad.Except (MonadIO (..), foldM, forM, forM_)
import qualified Data.ByteString.Char8 as BC
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.String (IsString (fromString))
import Database.PostgreSQL.Simple.Types (Query (..))

-----------------------------Template------------------------------------------
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
templateM str margs = return . template str <$$> margs

-----------------------------Monadic and simple functions----------------------
ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
  Just _ -> m
  _ -> return ()

ifNothing :: Monad m => Maybe a -> m () -> m ()
ifNothing ma m = case ma of
  Nothing -> m
  _ -> return ()

fromJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
fromJustM = forM_

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

readLnT :: MonadIO m => m String
readLnT = liftIO getLine

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f list = do
  mb <- forM list f
  return $ catMaybes mb

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

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
adjustM kl def k m = do
  let ma = M.lookup k m
  newa <- case ma of
    Just a -> kl a
    Nothing -> return def
  return $ M.insert k newa m

forMap :: M.Map k v -> (k -> w) -> M.Map k w
forMap mp f = M.mapWithKey (\k _ -> f k) mp

forMMem :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
forMMem cont b f = foldM f b cont

safeTail :: [a] -> [a]
safeTail [] = []
safeTail x = tail x

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

splitOnFst :: Eq a => a -> [a] -> ([a], [a])
splitOnFst _ [] = ([], [])
splitOnFst a xs | a `notElem` xs = ([], xs)
splitOnFst a (x : xs) | a == x = ([], xs)
splitOnFst a (x : xs) = let (b, c) = splitOnFst a xs in (x : b, c)

splitOnLast :: Eq a => a -> [a] -> ([a], [a])
splitOnLast a list = let (b, c) = splitOnFst a (reverse list) in (reverse c, reverse b)

_1of3 :: (a, b, c) -> a
_1of3 (a, _, _) = a

_12of3 :: (a, b, c) -> (a, b)
_12of3 (a, b, _) = (a, b)

_2of3 :: (a, b, c) -> b
_2of3 (_, b, _) = b

_3of3 :: (a, b, c) -> c
_3of3 (_, _, c) = c