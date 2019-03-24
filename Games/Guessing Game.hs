data Ending = Lose Int | Win deriving Show

guessingGame :: Int -> IO Ending
guessingGame secret | secret < 1 || secret > 100 = error "invalid game"
                    | otherwise = guessingGameRun secret 1 100

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