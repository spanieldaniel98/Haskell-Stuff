import Data.Char (digitToInt)

--Checks if an integer is a circular prime
circularPrime :: Int -> Bool
circularPrime n = and [ prime (listToInt xs) | xs <- cyclicPermutations(intToList n) 1 ]

--Returns list of factors of an integer
factors :: Int -> [Int]
factors n = [ x | x <- [1..n] , mod n x == 0 ]

--Checks if an integer is prime
prime :: Int -> Bool
prime n = factors n == [1,n]

--Returns a list of cyclic permutations of an integer represented as a list
cyclicPermutations :: [Int] -> Int -> [[Int]]
cyclicPermutations xs n | n == (length xs) = []
                        | otherwise = nextList : cyclicPermutations nextList (n+1)
						  where nextList = last xs : init xs

--Converts an integer to a list representation					  
intToList :: Int -> [Int]
intToList = map digitToInt . show

--Converts a list representation of an integer to an integer
listToInt :: [Int] -> Int
listToInt xs = foldl (\x y -> 10 * x + y) 0 xs