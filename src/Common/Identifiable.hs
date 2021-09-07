module Common.Identifiable where

import Data.List (find)

class Identifiable a where
  getId :: a -> Int

findById :: Identifiable a => Int -> [a] -> Maybe a
findById wantedId = find (\a -> getId a == wantedId)

-- | Set value only if find the element
setById :: Identifiable a => a -> [a] -> [a]
setById updated = map helper
  where
    wantedId = getId updated
    helper curValue = if getId curValue == wantedId then updated else curValue

-- * Should not break sort

updateInsertById :: Identifiable a => (a -> a) -> a -> [a] -> [a]
updateInsertById func updated list = res
  where
    wantedId = getId updated
    res = case findById wantedId list of
      Nothing -> list <> [updated]
      Just a -> setById (func a) list

-- | Filter only unique values
filterById :: Identifiable a => [a] -> [a]
filterById = foldl (\acc a -> if getId a `elem` map getId acc then acc else a : acc) []

-- | Universal function for concatenating rows
unite :: (Identifiable a) => (a -> a -> a) -> [a] -> [a]
unite f = foldl helper []
  where
    helper acc a = updateInsertById (f a) a acc
