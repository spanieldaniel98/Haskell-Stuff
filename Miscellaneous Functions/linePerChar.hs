linePerChar :: String -> IO ()
linePerChar "" = do putStr ""
linePerChar s = do putChar (head s)
                   putStrLn ""
                   linePerChar (tail s)