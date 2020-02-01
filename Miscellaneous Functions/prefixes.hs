-- take a list and return a list of lists, from the empty list 
-- to the full list, adding the elements to the list in order
prefixes :: [a] -> [[a]]
prefixes = reverse.prefixList

prefixList :: [a] -> [[a]]
prefixList [] = [[]]
prefixList xs = xs : prefixList (init xs)