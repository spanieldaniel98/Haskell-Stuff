-- *** 1: Permutations ***
-- check if list is a permutation of another
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation [] _      = False
isPermutation _ []      = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (remove1 x ys)

-- *** 2: Avoiding Duplicates ***
-- check if a list contains duplicates
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = elem x xs || hasDuplicates xs

-- remove duplicate elements from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) 
  | elem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs
  
-- *** 3: Pascal's Triangle ***
-- return pascal's triangle elements on nth level (starting from 1)
pascals 0 = []
pascals 1 = [1]
pascals n = 1 : [ x+y | (x,y) <- zip (pascals (n-1)) (tail (pascals (n-1))) ] ++ [1]

-- return elements of pascal's triangle on every level infinitely
infinitePascals = map pascals [1..]

-- *** 4: Erastosthenes' Sieve ***
-- remove all multiples of an integer from a list
crossOut :: Int -> [Int] -> [Int]
crossOut n xs = [ x | x <- xs , x `mod` n /= 0 ] 

-- Sieve of Eratosthenes
-- i.e return a list of all prime numbers up to a given limit
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs , y `mod` x /= 0 ]

-- return list of prime numbers up to 100
sieveTo100 :: [Int]
sieveTo100 = sieve [2..100]

-- *** 5: Number Games ***
-- return list of factors of a number
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

-- check if a number is prime
prime :: Integral a => a -> Bool
prime n = factors n == [1,n]

-- check if a number is prime and in range 2-100
isPrimeUpTo100 :: Integral a => a -> Bool
isPrimeUpTo100 n = 2 <= n && n <= 100 && prime n

-- return list of prime numbers up to a given limit
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [ x | x <- [1..n] , prime x ]

-- check if a number can be computed by summing two prime numbers in the range 2-100
isPrimeSum :: Integral a => a -> Bool
isPrimeSum n = length [ x | x <- primesUpTo 100  , y <- primesUpTo 100 , n == x + y ] > 0

-- *** 6: Occurences in Lists ***
-- check if element occurs in a list
occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = length [ y | y <- xs , y == x ] > 0

-- check if all elements in one list occur in another
allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn xs ys = and [ occursIn x ys | x <- xs ]

-- check if two lists contain exactly the same elements (order doesn't matter)
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccurIn xs ys && allOccurIn ys xs

-- return the number of times an element appears in a list
numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = length [ x' | x' <- xs , x' == x ]

-- *** 7: Elements and Positions ***
-- return a list of pairs of a list's elements and their indexes
positions :: (Num b, Enum b) => [a] -> [(a,b)]
positions xs = zip xs [0..]

-- return the first index at which an element occurs in a list
firstPosition :: (Num b, Enum b, Eq a) => a -> [a] -> b
firstPosition x xs = head [ y' | (x',y') <- positions xs , x' == x ]

-- remove the first instance of a given element from a list
remove1 :: Eq a => a -> [a] -> [a]
remove1 _ [] = []
remove1 x (y:ys) 
  | x == y    = ys
  | otherwise = y : remove1 x ys

-- remove n instances of a given element from a list (if that many exist)
remove :: Eq a => Int -> a -> [a] -> [a]	
remove 0 _ ys = ys
remove n x (y:ys)
  | x == y    = remove (n-1) x ys
  | otherwise = y : remove n x ys
  
-- *** 8: More List Comprehensions (triads) ***
-- return a list of Pythagorean triads up to a given limit
triads :: Int -> [(Int,Int,Int)]
triads n = [ (a,b,c) | c <- [1..n] , a <- [1..c] , b <- [a+1..c] , a^2 + b^2 == c^2 ]

-- return an infinite list of Pythagorean triads
infiniteTriads :: [(Int,Int,Int)]
infiniteTriads = [ (a,b,c) | c <- [1..] , a <- [1..c] , b <- [a+1..c] , a^2 + b^2 == c^2 ]