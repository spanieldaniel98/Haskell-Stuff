-- take a list, and return a list of lists, repeatedly tailing the list until it is empty
suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x:xs) = xs : suffixes xs