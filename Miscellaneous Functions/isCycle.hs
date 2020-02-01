module IsCycle where

-- determines whether a path represented by a list is a cycle or not
isCycle :: Ord a => [a] -> Bool
isCycle path = firstElem == lastElem
    where firstElem = path !! 0 
          lastElem = path !! ((length path) - 1)