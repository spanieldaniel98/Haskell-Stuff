module RemoveElem where

-- remove a single element from a list
removeElem :: Eq a => a -> [a] -> [a]
removeElem n [] = []
removeElem n (x:xs) | n == x = xs
                    | otherwise = x : removeElem n xs