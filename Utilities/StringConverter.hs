stringConvert :: String -> String -> (String -> String) -> (String -> String) -> (String -> String) -> IO String
stringConvert input goal f1 f2 f3 | input == goal = printSequence [] 
                                  | otherwise = stringConvert' goal f1 f2 f3 [(f1 input, [input, f1 input]),(f2 input, [input, f2 input]),(f3 input, [input, f3 input])]
                                       
stringConvert' :: String -> (String -> String) -> (String -> String) -> (String -> String) -> [(String, [String])] -> IO String 
stringConvert' goal f1 f2 f3 xs | ([ x | x <- xs , (fst x) == goal ] /= []) = printSequence (head [ (snd x) | x <- xs , (fst x) == goal ])
                                | otherwise = stringConvert' goal f1 f2 f3 ((applyAll f1 xs) ++ (applyAll f2 xs) ++ (applyAll f3 xs))

applyAll :: (String -> String) -> [(String, [String])] -> [(String, [String])]
applyAll f [] = []  
applyAll f (x:xs) = (result, (snd x)++[result]) : (applyAll f xs)
    where result = f(fst x)
    
exampleF1 :: String -> String
exampleF1 "" = ""
exampleF1 ('A':'A':xs) = "AB" ++ (exampleF1 xs)
exampleF1 (x:xs) = [x] ++ (exampleF1 xs)

exampleF2 :: String -> String
exampleF2 "" = ""
exampleF2 ('A':'B':xs) = "BB" ++ (exampleF2 xs)
exampleF2 (x:xs) = [x] ++ (exampleF2 xs)

exampleF3 :: String -> String
exampleF3 "" = ""
exampleF3 ('B':xs) = "AA" ++ (exampleF3 xs)
exampleF3 (x:xs) = [x] ++ (exampleF3 xs)
    
printSequence :: [String] -> IO String
printSequence xs = do let result = printSequence' xs
                      putStr "Method: "
                      putStrLn result
                      return result

printSequence' :: [String] -> String
printSequence' [] = []
printSequence' (x:[]) = x
printSequence' (x:xs) = x ++ " -> " ++ (printSequence' xs)