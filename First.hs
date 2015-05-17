import Test.QuickCheck

module Main where

data Weekday = Mo | Tue Int | Wed
	deriving (Read, Show)

-- When you compile and execute the file it will execute the main
-- function which is the line below
-- A function is meant to interact with the world. If it does not then
-- it is pointless. So IO will be what the world gives this function/file
-- IO is an abstract data type. You can't take it apart.
main :: IO ()
main = do 
	{ putStrLn "Hi "
	; str <- getLine -- Get the string but as an IO type 
	; putStrLn $ "You typed " ++ str
	-- putStrLn will pull the str out of the IO type from getLine
	-- NOTE: once you start dealing with IO you have to keep dealing with IO
	--	 until you escape from the function 
}
-- calculate the sum if all elements
sumL :: [Int] -> Int
sumL [] = 0
sumL (x:xs) = x + sumL xs

-- alternative implementation (tail recursive)
sumL' :: [Int] -> Int
sumL' xs = sumTL 0 xs  
  where 
    sumTL acc [] = acc
    sumTL acc (x:xs) = sumTL (acc + x) xs

nth:: [a] -> Int -> a
nth (x: _)  0 =  x
nth (_:xs)  n = nth xs (n-1)


-- list append (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys)

-- implementation of list reverse
rev :: [a] -> [a]
rev xs = rev' [] xs
  where
    rev' :: [a] -> [a] -> [a]
    rev' rxs [] = rxs
    rev' rxs (x:xs) = rev' (x:rxs) xs

-- testing reverse
test_rev_0 :: Bool
test_rev_0 = rev [1,2,3] == [3,2,1]

-- works, but has n^2 work complexity
revC [] = []
revC (x:xs) = (rev xs) ++ [x]

test_rev1 :: Eq a => [a] -> Bool
test_rev1 xs = (rev xs) == (revC xs)

test_rev2 :: [Int] -> Bool
test_rev2 xs = (length xs) == (length $ rev xs)

test_rev3 :: [Int] -> Bool
test_rev3 xs = rev(rev xs) == xs

test_rev4 :: [Int] -> [Int] -> Bool
test_rev4 xs ys = (rev ys ++ rev xs) == reverse (xs + ys)
