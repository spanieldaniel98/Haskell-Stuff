--from slides, can make my own version which gets a word from a database to guess, and only prints once etc.

import System.IO

main = hangman

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- secretGetLine
             let ds = replicate (length word) '-'
             putStrLn ds
             putStrLn "Try to guess it!"
             play word ds

secretGetLine :: IO String
secretGetLine = do hSetEcho stdin False
                   xs <- getLine
                   hSetEcho stdin True
                   return xs

play :: String -> String -> IO ()
play word answerSoFar | answerSoFar == word = do 
                                                putStrLn "Well done, you guessed the word!"
                                                putStrLn "Play again? (Y/N)"
                                                replay
                      | otherwise = do 
                                      putStrLn "Enter a single character:"
                                      guess <- getLine
                                      if length guess == 1 then 
                                        do updatedAnswer <- putUpdate (updateMatch word answerSoFar guess)
                                           play word updatedAnswer
	                                  else 
                                        play word answerSoFar

replay :: IO ()
replay = do 
           answer <- getLine
           if answer == "Y" then
		     do main
		   else if answer == "N" then
		     do putStrLn "Bye-bye!"
	       else
		     do putStrLn "Enter Y or N:"
		        replay

putUpdate :: String -> IO String
putUpdate s = do putStr "Your answer so far is: "
                 putStrLn s
                 return s

updateMatch :: String -> String -> [Char] -> String
updateMatch [] [] c = []
updateMatch (x:xs) (y:ys) [c] | x == y = x : updateMatch xs ys [c]
                              | x == c = x : updateMatch xs ys [c]
                              | otherwise = '-' : updateMatch xs ys [c]