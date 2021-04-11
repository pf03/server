module Identifiable where
import Data.List

class Identifiable a where
    getId :: a -> Int

findById :: Identifiable a => Int -> [a] -> Maybe a 
findById wantedId  = find (\a -> getId a == wantedId )

existId :: Identifiable a => Int -> [a] -> Bool
existId wantedId = any (\a -> getId a == wantedId)

--устанавливает только в случае, если этот элемент найден
setById :: Identifiable a => a -> [a] -> [a]
setById updated = map helper where
    wantedId = getId updated
    helper curValue = if getId curValue == wantedId then updated else curValue

--устанавливает в любом случае
insertById :: Identifiable a => a -> [a] -> [a]
insertById updated list = res where
    wantedId = getId updated
    res = if existId wantedId list 
        then setById updated list 
        else list <> [updated]

updateById :: Identifiable a => Int -> (a -> a) -> [a] -> [a]
updateById wantedId func = map helper where
    helper curValue = if getId curValue == wantedId then func curValue else curValue

--сортировка не должна нарушаться
updateInsertById :: Identifiable a => (a -> a) -> a -> [a] -> [a]
updateInsertById func updated list = res where
    wantedId = getId updated
    res = case  findById wantedId list of 
        Nothing -> list <> [updated]
        Just a -> setById (func a) list 

--оставляем только уникальные знчения
filterById :: Identifiable a => [a] -> [a]
filterById = foldl (\acc a -> if getId a `elem` map getId acc then acc else a:acc) []