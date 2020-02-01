-- take an input string and print each character thereof on a new line in the console
linePerChar :: String -> IO ()
linePerChar "" = do putStr ""
linePerChar s = do putChar (head s)
                   putStrLn ""
                   linePerChar (tail s)