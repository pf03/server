module Common.Identifiable where
import Data.List

class Identifiable a where
    getId :: a -> Int

findById :: Identifiable a => Int -> [a] -> Maybe a 
findById wantedId  = find (\a -> getId a == wantedId )

existId :: Identifiable a => Int -> [a] -> Bool
existId wantedId = any (\a -> getId a == wantedId)

-- | Set value only if find the element 
setById :: Identifiable a => a -> [a] -> [a]
setById updated = map helper where
    wantedId = getId updated
    helper curValue = if getId curValue == wantedId then updated else curValue

-- | Set value anyway
insertById :: Identifiable a => a -> [a] -> [a]
insertById updated list = res where
    wantedId = getId updated
    res = if existId wantedId list 
        then setById updated list 
        else list <> [updated]

updateById :: Identifiable a => Int -> (a -> a) -> [a] -> [a]
updateById wantedId func = map helper where
    helper curValue = if getId curValue == wantedId then func curValue else curValue

-- * Shouldn't break sort
updateInsertById :: Identifiable a => (a -> a) -> a -> [a] -> [a]
updateInsertById func updated list = res where
    wantedId = getId updated
    res = case  findById wantedId list of 
        Nothing -> list <> [updated]
        Just a -> setById (func a) list 

-- | Filter only unique values
filterById :: Identifiable a => [a] -> [a]
filterById = foldl (\acc a -> if getId a `elem` map getId acc then acc else a:acc) []