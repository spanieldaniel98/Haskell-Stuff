import System.IO
import Data.Char

main = teleKeypadConverter

blank :: IO ()
blank = putStrLn ""

teleKeypadConverter :: IO ()
teleKeypadConverter = do 
    blank
    putStrLn ">>>Telephone Keypad Converter<<<"
    blank
    play

play :: IO ()
play = do 
    putStrLn "Type 1 for English -> Telephone Keypad Digits, and 2 for Telephone Keypad Digits -> English: "
    input <- getLine
    blank
    if input == "1" then
      toTeleKeypad
    else if input == "2" then
      fromTeleKeypad
    else 
      play

toTeleKeypad :: IO ()
toTeleKeypad = do 
    putStrLn "Enter line to convert to telephone keypad digits (press Enter with no input to return to menu): "
    input <- getLine
    if input == "" then
      play
    else do 
      blank
      putStrLn $ id $ toDigits $ removeExtraSpaces input
      writeFile "output.txt" $ id $ toDigits $ removeExtraSpaces input
      blank
      toTeleKeypad 
                     
fromTeleKeypad :: IO ()
fromTeleKeypad = do 
    putStrLn "Enter line to convert from telephone keypad digits (press Enter with no input to return to menu): "
    input <- getLine
    if input == "" then
      play
    else do 
      blank
      putStrLn $ id $ fromDigits $ removeExtraSpaces input
      writeFile "output.txt" $ id $ fromDigits $ removeExtraSpaces input
      blank
      fromTeleKeypad 
                       
removeExtraSpaces :: String -> String
removeExtraSpaces "" = ""
removeExtraSpaces (' ' : ' ' : rest) = removeExtraSpaces (' ' : rest)
removeExtraSpaces (x:rest) = x : removeExtraSpaces rest

-- Converts an English string to telephone keypad digits                   
toDigits :: String -> String
toDigits "" = ""
toDigits [x] = toDigits' x
toDigits (x : ' ' : rest) = toDigits' x ++ " " ++ toDigits rest
toDigits (x : rest)   = toDigits' x ++ "-" ++ toDigits rest

toDigits' :: Char -> String
toDigits' 'a' = "2"
toDigits' 'b' = "22"
toDigits' 'c' = "222"
toDigits' 'd' = "3"
toDigits' 'e' = "33"
toDigits' 'f' = "333"
toDigits' 'g' = "4"
toDigits' 'h' = "44"
toDigits' 'i' = "444"
toDigits' 'j' = "5"
toDigits' 'k' = "55"
toDigits' 'l' = "555"
toDigits' 'm' = "6"
toDigits' 'n' = "66"
toDigits' 'o' = "666"
toDigits' 'p' = "7"
toDigits' 'q' = "77"
toDigits' 'r' = "777"
toDigits' 's'= "7777"
toDigits' 't' = "8"
toDigits' 'u' = "88"
toDigits' 'v' = "888"
toDigits' 'w' = "9"
toDigits' 'x' = "99"
toDigits' 'y' = "999"
toDigits' 'z' = "9999"
toDigits' '*' = "+"
toDigits' '_' = "0"
toDigits' '#' = "↑"
toDigits' _   = "!"

-- Converts a string of telephone keypad digits to English
fromDigits :: String -> String
fromDigits "" = ""
fromDigits (' ' : rest) = " " ++ fromDigits rest
fromDigits ('-' : rest)   = fromDigits rest
fromDigits ('2' : '2' : '2' : rest) = "c" ++ fromDigits rest
fromDigits ('2' : '2' : rest) = "b" ++ fromDigits rest
fromDigits ('2' : rest) = "a" ++ fromDigits rest
fromDigits ('3' : '3' : '3' : rest) = "f" ++ fromDigits rest
fromDigits ('3' : '3' : rest) = "e" ++ fromDigits rest
fromDigits ('3' : rest) = "d" ++ fromDigits rest
fromDigits ('4' : '4' : '4' : rest) = "i" ++ fromDigits rest
fromDigits ('4' : '4' : rest) = "h" ++ fromDigits rest
fromDigits ('4' : rest) = "g" ++ fromDigits rest
fromDigits ('5' : '5' : '5' : rest) = "l" ++ fromDigits rest
fromDigits ('5' : '5' : rest) = "k" ++ fromDigits rest
fromDigits ('5' : rest) = "j" ++ fromDigits rest
fromDigits ('6' : '6' : '6' : rest) = "o" ++ fromDigits rest
fromDigits ('6' : '6' : rest) = "n" ++ fromDigits rest
fromDigits ('6' : rest) = "m" ++ fromDigits rest
fromDigits ('7' : '7' : '7' : '7' : rest) = "s" ++ fromDigits rest
fromDigits ('7' : '7' : '7' : rest) = "r" ++ fromDigits rest
fromDigits ('7' : '7' : rest) = "q" ++ fromDigits rest
fromDigits ('7' : rest) = "p" ++ fromDigits rest
fromDigits ('8' : '8' : '8' : rest) = "v" ++ fromDigits rest
fromDigits ('8' : '8' : rest) = "u" ++ fromDigits rest
fromDigits ('8' : rest) = "t" ++ fromDigits rest
fromDigits ('9' : '9' : '9' : '9' : rest) = "z" ++ fromDigits rest
fromDigits ('9' : '9' : '9' : rest) = "y" ++ fromDigits rest
fromDigits ('9' : '9' : rest) = "x" ++ fromDigits rest
fromDigits ('9' : rest) = "w" ++ fromDigits rest
fromDigits ('*' : rest) = "+" ++ fromDigits rest
fromDigits ('0' : rest) = "_" ++ fromDigits rest
fromDigits ('↑' : rest) = "#" ++ fromDigits rest
fromDigits (_ : rest) = "!" ++ fromDigits rest