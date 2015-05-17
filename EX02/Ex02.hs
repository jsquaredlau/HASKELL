module Ex02 (
  BinaryTree(..), isBST, insert, deleteAll, searchTrees, isSorted, sortedListsWithoutDuplicates, isBalanced,
  mysteryPred, mysterious, astonishing
) where

import Test.QuickCheck
import Data.List(sort, nub)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) = allTree (< v) l && allTree (>= v) r && isBST l && isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True

--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r)
  | i < v = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where
   searchTrees' 0 = return Leaf
   searchTrees' n = do
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

-- This is a check of whether or not the given integer is in the tree
mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred i Leaf = False
mysteryPred i (Branch j l r)
  | i < j = mysteryPred i l
  | i > j = mysteryPred i r
  | otherwise = True

----------------------

-- Gets all of the numbers from a BST and returns them as an array
mysterious :: BinaryTree -> [Integer]
list = []
mysterious Leaf = []
mysterious (Branch i l r) = depthFirst (Branch i l r)
  where depthFirst :: BinaryTree -> [Integer]
        depthFirst Leaf = []
        depthFirst (Branch i l Leaf) = depthFirst l ++ [i]
        depthFirst (Branch i Leaf r) = [i] ++ depthFirst r
        depthFirst (Branch i l r) = depthFirst l ++ [i] ++ depthFirst r


isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

----------------------

-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing [x] = (Branch x Leaf Leaf)
astonishing [x,y]
  | x < y = Branch x Leaf (Branch y Leaf Leaf)
  | otherwise = Branch x (Branch y Leaf Leaf) Leaf
astonishing list = (sortedArrayToBST list 0 ((length list)-1))
  where sortedArrayToBST :: [Integer] -> Int -> Int -> BinaryTree
        sortedArrayToBST list start end
          | start > end = Leaf
          | otherwise = do
              let mid = start + ((end - start) `div` 2)
              (Branch (list!!mid)  (sortedArrayToBST list start (mid-1)) (sortedArrayToBST list (mid+1) end) )

isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)

---------------------
-- For part 1a
-- prop_mysteryPred_1 integer = forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)
-- prop_mysteryPred_2 integer = forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))

---------------------
-- For part 1b
-- prop_mysterious_1 integer = forAll searchTrees $ \tree -> mysteryPred integer tree == (integer `elem` mysterious tree)
-- prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious

---------------------
-- For part 2
-- prop_sortedListsWithoutDuplicates_1 = forAll sortedListsWithoutDuplicates isSorted
-- prop_sortedListsWithoutDuplicates_2 = forAll sortedListsWithoutDuplicates $ \x -> x == nub x
prop_astonishing_1 = forAll sortedListsWithoutDuplicates $ isBST . astonishing
prop_astonishing_2 = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing
prop_astonishing_3 = forAll sortedListsWithoutDuplicates $ \ integers -> mysterious (astonishing integers) == integers
---------------------
