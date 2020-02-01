-- return bondings from a given predicate and list
findBonding :: Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding _ [] = Just []
findBonding f xs | odd (length xs) = Nothing
                 | bond == []      = Nothing
                 | otherwise       = Just (head bond)
                   where bond      = [ x | x <- perms f xs , length x == length xs ]

-- return list of unique pairs satisfying a predicate
pairs :: Eq a => (a -> a -> Bool) -> [a] -> [(a,a)]
pairs f xs = rmSwaps [ (x,y) | x <- xs , y <- xs , x /= y , f x y , f y x ]

-- remove elements (x,y) from a list where (y,x) is also in the list 
-- (i.e. remove swapped element pairs)
rmSwaps :: Eq a => [(a,a)] -> [(a,a)]
rmSwaps []         = []
rmSwaps ((x,y):xs) | elem (y,x) xs = rmSwaps xs
                   | otherwise     = (x,y) : rmSwaps xs

-- return all permutations of unique pair list
perms :: Eq a => (a -> a -> Bool) -> [a] -> [[(a,a)]]
perms _ [] = [[]]
perms f xs = [ (x,y):(y,x):rest | (x,y) <- pairs f xs , rest <- perms f (rm x $ rm y xs) ] 

-- remove an element from a list
rm :: Eq a => a -> [a] -> [a]
rm _ []     = []
rm x (y:xs) 
  | x == y    = xs
  | otherwise = y : (rm x xs)