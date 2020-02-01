data VTree a = Leaf | Node (VTree a) a Int (VTree a) deriving Show
data Direction a = L a Int (VTree a) | R a Int (VTree a) deriving Show
type Trail a     = [Direction a]
type Zipper a    = (VTree a, Trail a)

-- take a value and a zipper, and insert a new node with the given value into the tree
insertFromCurrentNode :: Ord a => a -> Zipper a -> Zipper a
insertFromCurrentNode n zipper = insert n (goRoot zipper)

-- insert an element into a tree zipper
insert :: Ord a => a -> Zipper a -> Zipper a
insert n (Leaf, trail) = (Node Leaf n 1 Leaf, trail)
insert n (Node l a x r, trail) 
  | n == a = (Node l a x r, trail)
  | n < a && nonEmpty trail && n < parentValue = insert n parentNode
  | n > a && nonEmpty trail && n > parentValue = insert n parentNode
  | n < a     = insert n leftNode
  | otherwise = insert n rightNode
    where parentValue = nodeValue(goUp(Node l a x r, trail))
          parentNode  = increment(goUp(Node l a x r, trail))
          leftNode    = increment(goLeft(Node l a x r, trail))
          rightNode   = increment(goRight(Node l a x r, trail))

-- check if a list is empty
nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _ = True

-- go to the root in a tree (in a zipper)
goRoot :: Zipper a -> Zipper a
goRoot (tree, []) = (tree, [])
goRoot (tree, trail) = goRoot(increment(goUp(tree, trail)))
                                                      
-- go up one node in a tree (in a zipper)
goUp :: Zipper a -> Zipper a
goUp (l, (L a x r :trail)) = (Node l a x r, trail)
goUp (r, (R a x l :trail)) = (Node l a x r, trail)

-- go left one node in a tree (in a zipper)                    
goLeft :: Zipper a -> Zipper a
goLeft (Node l a x r, trail) = (l, (L a x r :trail))

-- go right one node in a tree (in a zipper)     
goRight :: Zipper a -> Zipper a
goRight (Node l a x r, trail) = (r, (R a x l :trail))

-- increment the current node's counter
increment :: Zipper a -> Zipper a
increment (Leaf, trail)         = (Leaf, trail)
increment (Node l a x r, trail) = (Node l a (x+1) r, trail)

-- return the current node's value
nodeValue :: Zipper a -> a
nodeValue (Node l a x r, trail) = a

-- create a tree zipper from a list (useful for testing)
mkTree :: Ord a => [a] -> Zipper a
mkTree = foldl (\z -> \x -> insertFromCurrentNode x z) (Leaf,[])