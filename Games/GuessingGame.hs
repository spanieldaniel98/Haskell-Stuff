import System.IO

data Ending = Lose Int | Win deriving Show

main = guessingGame

blank :: IO ()
blank = putStrLn ""

guessingGame :: IO Ending
guessingGame = do 
  blank 
  putStrLn ">>>Welcome to the Guessing Game!<<<"
  blank
  putStrLn "Enter an upper limit to guess up to (e.g. 100):"
  limit <- getLine
  getNumber (read limit)

getNumber :: Int -> IO Ending
getNumber limit = do
  putStrLn "Enter a secret number to guess (which won't show as you type):"
  hSetEcho stdin False
  secret <- getLine
  hSetEcho stdin True
  
  if (read secret) < 1 then 
    do
      putStrLn ("Make sure to enter a number between 1 and " ++ show limit ++ "!")
      getNumber limit
  else if (read secret) > limit then 
    do
      putStrLn ("Make sure to enter a number between 1 and " ++ show limit ++ "!")
      getNumber limit
  else
    guessingGameRun (read secret) 1 limit

guessingGameRun :: Int -> Int -> Int -> IO Ending
guessingGameRun secret lowerBound upperBound = do putStrLn "Guess the number!"
                                                  putStr "The lower bound is "
                                                  print lowerBound
                                                  putStr "The upper bound is "
                                                  print upperBound

                                                  x <- getLine
                                                  if x == "Surrender" then
                                                    do putStrLn "Sorry, you lost..."
                                                       putStr "The secret answer was "
                                                       print secret
                                                       return (Lose secret)
                                                  else continue (read x) secret lowerBound upperBound
  
continue ::  Int -> Int -> Int -> Int -> IO Ending
continue x secret lowerBound upperBound | (x < lowerBound) || (x > upperBound) = guessingGameRun secret lowerBound upperBound
                                        | x == secret                          = do putStrLn "Congrats, you win!"
                                                                                    return Win
                                        | x < secret                           = guessingGameRun secret (x+1) upperBound
                                        | otherwise                            = guessingGameRun secret lowerBound (x-1)