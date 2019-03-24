prefixes :: [a] -> [[a]]
prefixes = reverse.prefixList

prefixList :: [a] -> [[a]]
prefixList [] = [[]]
prefixList xs = xs : prefixList (init xs)