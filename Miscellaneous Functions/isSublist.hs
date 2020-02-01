-- take two lists and determine if the former is a sublist of the latter (returning a boolean)
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist a b = elem a (powerSet b)

-- take an input list of elements and return the powerset thereof
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [ x:ns | ns <- powerSet xs ] ++ powerSet xs