module Common.Functions where

import Control.Monad.Except (MonadIO (..), forM)
import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)

ifJust :: Monad m => Maybe a -> m () -> m ()
ifJust ma m = case ma of
  Just _ -> m
  _ -> return ()

printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

putStrLnT :: MonadIO m => String -> m ()
putStrLnT = liftIO . putStrLn

readLnT :: MonadIO m => m String
readLnT = liftIO getLine

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f list = do
  mb <- forM list f
  return $ catMaybes mb

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

jLookup :: Eq a => a -> [(a, b)] -> b
jLookup key list = fromJust $ lookup key list

(<<$>>) :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
(<<$>>) f mna = do
  na <- mna
  return $ f <$> na

infixl 4 <<$>>

(<$$>) :: Monad m => ([a] -> m b) -> [m a] -> m b
(<$$>) f list = sequenceA list >>= f

infixl 4 <$$>

forMapWithKeyM :: Monad m => M.Map k v -> (k -> v -> m w) -> m (M.Map k w)
forMapWithKeyM mp f = sequence $ M.mapWithKey f mp

adjustM :: Monad m => Ord k => (a -> m a) -> a -> k -> M.Map k a -> m (M.Map k a)
adjustM kl def k m = do
  let ma = M.lookup k m
  newa <- case ma of
    Just a -> kl a
    Nothing -> return def
  return $ M.insert k newa m

splitOnFst :: Eq a => a -> [a] -> ([a], [a])
splitOnFst _ [] = ([], [])
splitOnFst a xs | a `notElem` xs = ([], xs)
splitOnFst a (x : xs) | a == x = ([], xs)
splitOnFst a (x : xs) = let (b, c) = splitOnFst a xs in (x : b, c)

splitOnLast :: Eq a => a -> [a] -> ([a], [a])
splitOnLast a list = let (b, c) = splitOnFst a (reverse list) in (reverse c, reverse b)

-- For JSON correct parsing
deletePrefixOptions :: Int -> Options
deletePrefixOptions n = defaultOptions {fieldLabelModifier = deletePrefix n}

deletePrefix :: Int -> String -> String
deletePrefix n str = case drop n str of
  x : xs -> toLower x : xs
  [] -> []