isSublist :: Eq a => [a] -> [a] -> Bool
isSublist a b = elem a (powerSet b)

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [ x:ns | ns <- powerSet xs ] ++ powerSet xs