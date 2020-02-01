module ContainsDuplicates where

import RemoveElem

-- check if a given list contains duplicates
containsDuplicates :: Eq a => [a] -> Bool
containsDuplicates list = or [ elem n (removeElem n list) | n <- list ]