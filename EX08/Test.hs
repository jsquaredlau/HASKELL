import System.Random
import Control.Applicative
import System.Environment

generateTree :: Int -> IO (BinaryTree Int)
generateTree 0 = return Leaf
generateTree height = Branch <$> (randomIO' :: IO Int) <*> generateTree (height-1) <*> generateTree (height-1)
  where
    randomIO' = ((`mod` 100000) . abs) <$> randomIO

sumTree :: BinaryTree Int -> Int
sumTree Leaf           = 0
sumTree (Branch v l r) = sumTree l + v + sumTree r

fac n = fac' 1 n
  where
    fac' :: Int -> Int -> Int
    fac' a 0 = a
    fac' a n = fac' (a * n) (n - 1)
