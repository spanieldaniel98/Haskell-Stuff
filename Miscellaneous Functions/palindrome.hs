-- check if a list/string is a palindrome or not
palindrome :: Eq a => [a] -> Bool
palindrome []  = True
palindrome [_] = True
palindrome xs = (head xs) == (last xs) && palindrome (tail (init xs))