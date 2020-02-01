main = morse

blank :: IO ()
blank = putStrLn ""

morse :: IO ()
morse = do blank
           putStrLn ">>>Morse Code Converter<<<"
           blank
           play
           
play :: IO ()
play = do putStrLn "Type 1 for English -> Morse, and 2 for Morse -> English: "
          input <- getLine
          blank
          if input == "1" then
            toMorse
          else if input == "2" then
            fromMorse
          else 
            play

toMorse :: IO ()
toMorse = do putStrLn "Enter line to convert to morse code (press Enter with no input to return to menu): "
             input <- getLine
             if input == "" then
                play
             else do blank
                     putStrLn $ id (morsify input)
                     blank
                     toMorse
                     
fromMorse :: IO ()
fromMorse = do putStrLn "Enter line to convert from morse code (press Enter with no input to return to menu): "
               input <- getLine
               if input == "" then
                  play
               else do blank
                       putStrLn $ id (removeExtraSpaces $ unmorsify input)
                       blank
                       fromMorse
                       
removeExtraSpaces :: String -> String
removeExtraSpaces "" = ""
removeExtraSpaces (' ' : ' ' : rest) = removeExtraSpaces (' ' : rest)
removeExtraSpaces (x:rest) = x : removeExtraSpaces rest

--Converts an English string to Morse code                       
morsify :: String -> String

--empty string
morsify "" = ""

--spaces
morsify (' ' : ' ' : rest) = morsify (' ' : rest)
morsify (' ' : rest) = " / " ++ morsify rest

--5's (numbers)
morsify ('1' : rest) = ".---- " ++ morsify rest
morsify ('2' : rest) = "..--- " ++ morsify rest
morsify ('3' : rest) = "...-- " ++ morsify rest
morsify ('4' : rest) = "....- " ++ morsify rest
morsify ('5' : rest) = "..... " ++ morsify rest
morsify ('6' : rest) = "-.... " ++ morsify rest
morsify ('7' : rest) = "--... " ++ morsify rest
morsify ('8' : rest) = "---.. " ++ morsify rest
morsify ('9' : rest) = "----. " ++ morsify rest
morsify ('0' : rest) = "----- " ++ morsify rest

--4's
morsify ('B' : rest) = "-... " ++ morsify rest
morsify ('b' : rest) = "-... " ++ morsify rest
morsify ('C' : rest) = "-.-. " ++ morsify rest
morsify ('c' : rest) = "-.-. " ++ morsify rest
morsify ('F' : rest) = "..-. " ++ morsify rest
morsify ('f' : rest) = "..-. " ++ morsify rest
morsify ('H' : rest) = ".... " ++ morsify rest
morsify ('h' : rest) = ".... " ++ morsify rest
morsify ('J' : rest) = ".--- " ++ morsify rest
morsify ('j' : rest) = ".--- " ++ morsify rest
morsify ('L' : rest) = ".-.. " ++ morsify rest
morsify ('l' : rest) = ".-.. " ++ morsify rest
morsify ('P' : rest) = ".--. " ++ morsify rest
morsify ('p' : rest) = ".--. " ++ morsify rest
morsify ('Q' : rest) = "--.- " ++ morsify rest
morsify ('q' : rest) = "--.- " ++ morsify rest
morsify ('V' : rest) = "...- " ++ morsify rest
morsify ('v' : rest) = "...- " ++ morsify rest
morsify ('X' : rest) = "-..- " ++ morsify rest
morsify ('x' : rest) = "-..- " ++ morsify rest
morsify ('Y' : rest) = "-.-- " ++ morsify rest
morsify ('y' : rest) = "-.-- " ++ morsify rest
morsify ('Z' : rest) = "--.. " ++ morsify rest
morsify ('z' : rest) = "--.. " ++ morsify rest

--3's
morsify ('D' : rest) = "-.. " ++ morsify rest
morsify ('d' : rest) = "-.. " ++ morsify rest
morsify ('G' : rest) = "--. " ++ morsify rest
morsify ('g' : rest) = "--. " ++ morsify rest
morsify ('K' : rest) = "-.- " ++ morsify rest
morsify ('k' : rest) = "-.- " ++ morsify rest
morsify ('O' : rest) = "--- " ++ morsify rest
morsify ('o' : rest) = "--- " ++ morsify rest
morsify ('R' : rest) = ".-. " ++ morsify rest
morsify ('r' : rest) = ".-. " ++ morsify rest
morsify ('S' : rest) = "... " ++ morsify rest
morsify ('s' : rest) = "... " ++ morsify rest
morsify ('U' : rest) = "..- " ++ morsify rest
morsify ('u' : rest) = "..- " ++ morsify rest
morsify ('W' : rest) = ".-- " ++ morsify rest
morsify ('w' : rest) = ".-- " ++ morsify rest

--2's
morsify ('A' : rest) = ".- " ++ morsify rest
morsify ('a' : rest) = ".- " ++ morsify rest
morsify ('I' : rest) = ".. " ++ morsify rest
morsify ('i' : rest) = ".. " ++ morsify rest
morsify ('M' : rest) = "-- " ++ morsify rest
morsify ('m' : rest) = "-- " ++ morsify rest
morsify ('N' : rest) = "-. " ++ morsify rest
morsify ('n' : rest) = "-. " ++ morsify rest

--1's
morsify ('E' : rest) = ". " ++ morsify rest
morsify ('e' : rest) = ". " ++ morsify rest
morsify ('T' : rest) = "- " ++ morsify rest
morsify ('t' : rest) = "- " ++ morsify rest

--other (error) characters
morsify (_ : rest) = "! " ++ morsify rest


--Converts a Morse code string to English
unmorsify :: String -> String

--empty string
unmorsify "" = ""

--spaces
unmorsify (' ' : rest) = unmorsify rest
unmorsify ('/' : rest) = " " ++ unmorsify rest

--5's (numbers)
unmorsify ('.' : '-' : '-' : '-' : '-' : rest) = "1" ++ unmorsify rest
unmorsify ('.' : '.' : '-' : '-' : '-' : rest) = "2" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : '-' : '-' : rest) = "3" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : '.' : '-' : rest) = "4" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : '.' : '.' : rest) = "5" ++ unmorsify rest
unmorsify ('-' : '.' : '.' : '.' : '.' : rest) = "6" ++ unmorsify rest
unmorsify ('-' : '-' : '.' : '.' : '.' : rest) = "7" ++ unmorsify rest
unmorsify ('-' : '-' : '-' : '.' : '.' : rest) = "8" ++ unmorsify rest
unmorsify ('-' : '-' : '-' : '-' : '.' : rest) = "9" ++ unmorsify rest
unmorsify ('-' : '-' : '-' : '-' : '-' : rest) = "0" ++ unmorsify rest

--4's
unmorsify ('-' : '.' : '.' : '.' : rest) = "B" ++ unmorsify rest
unmorsify ('-' : '.' : '-' : '.' : rest) = "C" ++ unmorsify rest
unmorsify ('.' : '.' : '-' : '.' : rest) = "F" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : '.' : rest) = "H" ++ unmorsify rest
unmorsify ('.' : '-' : '-' : '-' : rest) = "J" ++ unmorsify rest
unmorsify ('.' : '-' : '.' : '.' : rest) = "L" ++ unmorsify rest
unmorsify ('.' : '-' : '-' : '.' : rest) = "P" ++ unmorsify rest
unmorsify ('-' : '-' : '.' : '-' : rest) = "Q" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : '-' : rest) = "V" ++ unmorsify rest
unmorsify ('-' : '.' : '.' : '-' : rest) = "X" ++ unmorsify rest
unmorsify ('-' : '.' : '-' : '-' : rest) = "Y" ++ unmorsify rest
unmorsify ('-' : '-' : '.' : '.' : rest) = "Z" ++ unmorsify rest

--3's
unmorsify ('-' : '.' : '.' : rest) = "D" ++ unmorsify rest
unmorsify ('-' : '-' : '.' : rest) = "G" ++ unmorsify rest
unmorsify ('-' : '.' : '-' : rest) = "K" ++ unmorsify rest
unmorsify ('-' : '-' : '-' : rest) = "O" ++ unmorsify rest
unmorsify ('.' : '-' : '.' : rest) = "R" ++ unmorsify rest
unmorsify ('.' : '.' : '.' : rest) = "S" ++ unmorsify rest
unmorsify ('.' : '.' : '-' : rest) = "U" ++ unmorsify rest
unmorsify ('.' : '-' : '-' : rest) = "W" ++ unmorsify rest

--2's
unmorsify ('.' : '-' : rest) = "A" ++ unmorsify rest
unmorsify ('.' : '.' : rest) = "I" ++ unmorsify rest
unmorsify ('-' : '-' : rest) = "M" ++ unmorsify rest
unmorsify ('-' : '.' : rest) = "N" ++ unmorsify rest

--1's
unmorsify ('.' : rest) = "E" ++ unmorsify rest
unmorsify ('-' : rest) = "T" ++ unmorsify rest

----other (error) characters
unmorsify (_ : rest) = "!" ++ unmorsify rest

--have manual / howtouse at start