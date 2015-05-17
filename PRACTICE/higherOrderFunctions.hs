module HigherOrderFunction (compareWithHundred) where

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  

subtractFour :: (Num a, Ord a) => a -> a
subtractFour = (subtract 4)
