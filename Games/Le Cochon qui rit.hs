import System.IO
import Control.Monad.Random

main = gameStart

blank :: IO ()
blank = putStrLn ""
                  
gameStart :: IO ()
gameStart = do blank
               putStrLn "============================="
               putStrLn "Welcome to Le Cochon qui rit!"
               putStrLn "============================="
               blank
               choosePlayers
               
choosePlayers :: IO ()
choosePlayers = do putStrLn "Enter a number of players (1-4): "
                   choosePlayers'

choosePlayers' :: IO ()                  
choosePlayers' = do numOfPlayers <- getLine
                    case numOfPlayers of
                      "1" -> play 1
                      "2" -> play 2
                      "3" -> play 3
                      "4" -> play 4
                      _ -> do putStrLn "Please enter a number from 1-4" 
                              choosePlayers'
                        
play :: Int -> IO ()
play x = do blank
            putStrLn "tbc"