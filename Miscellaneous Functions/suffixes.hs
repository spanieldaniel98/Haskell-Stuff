suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:xs) = xs : suffixes xs