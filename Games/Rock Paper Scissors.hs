import System.IO
import Control.Monad.Random

main = rps

blank :: IO ()
blank = putStrLn ""

rps :: IO ()
rps = do blank
         putStrLn "===================================================="
         putStrLn "Welcome to Rock Paper Scissors, \"Best of 3 Edition\"!"
         putStrLn "===================================================="
         blank
         play 0 0
         
play :: Int -> Int -> IO ()
play w l = do putStrLn "Type 'r' for rock, 'p' for paper or 's' for scissors: "

              x <- getLine
              if ((x !! 0) /= 'r') && ((x !! 0) /= 'p') && ((x !! 0) /= 's') then
                do blank
                   play w l
              else
                do y <- comChoice

                   blank
                   putStr "The computer chose "
                   putStr $ id (fullString y)
                   putStr "... "
              
                   let outcome = result (x !! 0) y
                   let s = first outcome
                   let wins = w + (second outcome)
                   let losses = l + (third outcome)

                   putStrLn s

                   blank
                   putStr "Wins: "
                   putStr.show $ wins
                   putStr " | Losses: "
                   print losses
              
                   bestOfThree wins losses

                   play wins losses

comChoice :: IO Char
comChoice = do i <- getRandomR (0, 2)
               return (['r','p','s'] !! i)

fullString :: Char -> String
fullString 'r' = "rock"
fullString 'p' = "paper"
fullString 's' = "scissors"

result :: Char -> Char -> (String,Int,Int)
result x y | x == y = ("It's a tie!",0,0)
           | (x == 'r' && y == 's') || (x == 'p' && y == 'r') || (x == 's' && y == 'p') = ("You win!",1,0)
           | otherwise = ("Sorry, you lose...",0,1)
           
first :: (String,Int,Int) -> String
first (s,_,_) = s

second :: (String,Int,Int) -> Int
second (_,n,_) = n

third :: (String,Int,Int) -> Int
third (_,_,n) = n

bestOfThree :: Int -> Int -> IO ()
bestOfThree x y | (x + y) == 3 && x > y = do blank
                                             putStrLn "~~~~~~~~~~~~~~~~~~~~"
                                             putStrLn "!!!Victory royale!!!"
                                             putStrLn "~~~~~~~~~~~~~~~~~~~~"
                                             blank
                                             putStrLn "======================"
                                             putStrLn "Time for a new game..."
                                             putStrLn "======================"
                                             blank
                                             play 0 0
                | (x + y) == 3 && x < y = do blank
                                             putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                                             putStrLn "Tooooooo Baaaaaaad!!"
                                             putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                                             blank
                                             putStrLn "======================"
                                             putStrLn "Time for a new game..."
                                             putStrLn "======================"
                                             blank
                                             play 0 0
                | otherwise = putStrLn ""
         
replay :: Int -> Int -> IO ()
replay w l = do putStrLn "Play again? (Y/N)"
                answer <- getLine
                if answer == "Y" || answer == "y" then
		          do play w l
		        else if answer == "N" || answer == "n" then
		          do putStrLn "Bye-bye!"
                else
                  do putStrLn "Enter Y or N:"
                     replay w l