import Control.Monad.Random

data Coin = Heads | Tails deriving Show

coinFlip :: IO Coin
coinFlip = do i <- getRandomR (0, 1) :: IO Int
              case i of
                0 -> return Heads
                _ -> return Tails