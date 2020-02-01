import Control.Monad.Random

-- data type denoting a coin side (heads or tails)
data Coin = Heads | Tails deriving Show

-- flip a virtual coin, returning heads or tails
coinFlip :: IO Coin
coinFlip = do i <- getRandomR (0, 1) :: IO Int
              case i of
                0 -> return Heads
                _ -> return Tails