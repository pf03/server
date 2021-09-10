module Common.Functions where

import Control.Monad.Except (forM)
import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f list = do
  mb <- forM list f
  return $ catMaybes mb

(<<$>>) :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
(<<$>>) f mna = do
  na <- mna
  return $ f <$> na

infixl 4 <<$>>

forMapWithKeyM :: Monad m => M.Map k v -> (k -> v -> m w) -> m (M.Map k w)
forMapWithKeyM mp f = sequence $ M.mapWithKey f mp

adjustM :: Monad m => Ord k => (a -> m a) -> a -> k -> M.Map k a -> m (M.Map k a)
adjustM kl def k m = do
  let ma = M.lookup k m
  newa <- case ma of
    Just a -> kl a
    Nothing -> return def
  return $ M.insert k newa m

breakOnLast :: Eq a => a -> [a] -> ([a], [a])
breakOnLast a list = let (b, c) = break (== a) (reverse list) in (reverse c, reverse b)

-- For JSON correct parsing
deletePrefixOptions :: Int -> Options
deletePrefixOptions n = defaultOptions {fieldLabelModifier = deletePrefix n}

deletePrefix :: Int -> String -> String
deletePrefix n str = case drop n str of
  x : xs -> toLower x : xs
  [] -> []