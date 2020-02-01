-- return an infinite list of fibonacci numbers
fibonacci :: [Int]
fibonacci = 0 : 1 : [ x+y | (x,y) <- zip fibonacci (tail fibonacci) ]

-- return a fibonacci sequence up to n numbers
fibonacciUpTo :: Int -> [Int]
fibonacciUpTo n = take n fibonacci

-- return the Lucas sequence (infinite sequence)
lucasSeq :: [Int]
lucasSeq = 2 : 1 : [ x+y | (x,y) <- zip lucasSeq (tail lucasSeq) ]

-- return a Lucas sequence up to n numbers
lucasSeqUpTo :: Int -> [Int]
lucasSeqUpTo n = take n lucasSeq