import System.IO

--needa finish putting words in the files!
data Kangaroo = KangarooWord | AntiKangarooWord | NotKangarooWord deriving Show

kangaroo :: String -> IO Kangaroo
kangaroo s = do 
    kWords <- readFile "kangarooWords.txt"
    let kangarooWords = words kWords
    antiKWords <- readFile "antiKangarooWords.txt"
    let notKangarooWords = words antiKWords
    
    if elem s kangarooWords then
      return KangarooWord
    else if elem s antiKangarooWords then
      return AntiKangarooWord
    else return NotKangarooWord