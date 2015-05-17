module Ex03 (
  BinaryTree(..), isBST, insertBST, searchTrees,
  prop_insert_on_empty_tree, prop_insert_preserves_bst, prop_insert_adds_element, prop_insert_does_not_change_other_elements,
  prop_insert_duplicate_check,
  prop_delete_detect_error, prop_delete_preserves_bst, prop_delete_removes_element, prop_delete_does_not_change_other_elements,
  height, size
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

insertBST :: Integer -> BinaryTree -> BinaryTree
insertBST i Leaf = Branch i Leaf Leaf
insertBST i (Branch v l r)
  | i < v = Branch v (insertBST i l) r
  | otherwise = Branch v l (insertBST i r)

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where
   searchTrees' 0 = return Leaf
   searchTrees' n = do
      v <- (arbitrary :: Gen Integer)
      fmap (insertBST v) (searchTrees' $ n - 1)

-- This is a check of whether or not the given integer is in the tree
isInBST :: Integer -> BinaryTree -> Bool
isInBST i Leaf = False
isInBST i (Branch j l r)
  | i < j = isInBST i l
  | i > j = isInBST i r
  | otherwise = True

hasDuplicate :: Integer -> BinaryTree -> Bool
hasDuplicate i Leaf = False
hasDuplicate i (Branch j l r)
  | i < j = hasDuplicate i l
  | i > j = hasDuplicate i r
  | otherwise = (childIsNotDupe i r) && (childIsNotDupe i l)

childIsNotDupe :: Integer -> BinaryTree -> Bool
childIsNotDupe i Leaf = True
childIsNotDupe i (Branch q l r)
  | i == q = False
  | otherwise = True

--Remove all instances of an integer in a binary tree, preserving BST property
-- deleteAll :: Integer -> BinaryTree -> BinaryTree
-- deleteAll i Leaf = Leaf
-- deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
-- deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
-- deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
--                                        in Branch x l' (deleteAll i r)
--                            | i <  j = Branch j (deleteAll i l) r
--                            | i >  j = Branch j l (deleteAll i r)
--   where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
--         deleteRightmost (Branch i l Leaf) = (i, l)
--         deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
--                                              in (x, Branch i l r')

-- If the tree is empty, deletion is easy:
treeDelete :: Integer -> BinaryTree -> BinaryTree
treeDelete x Leaf = Leaf

-- If there's a branch
treeDelete x (Branch y left right) =
    -- maybe have to delete it from a subtree
    if x < y then Branch y (treeDelete x left) right
    else if x > y then Branch y left (treeDelete x right)
    else -- x == y, have to delete it from this node in the middle, which is trickier
        case left of
            -- If the left subtree is empty, we're deleting the root
            Leaf ->
                right
            branch ->
                let
                    (m, newLeft) = treePop left
                in
                    Branch m newLeft right

-- Delete the right-most (maximum element) from the tree, returning it and the new tree
treePop :: BinaryTree -> (Integer, BinaryTree)
treePop (Branch y left Leaf) =
    (y, left)

treePop (Branch x left right) =
    let
        (z, newRight) = treePop right
    in
        (z, Branch x left newRight)

--------------

prop_insert_on_empty_tree :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Bool
prop_insert_on_empty_tree insertFunction integer = (insertFunction integer Leaf) == Branch integer Leaf Leaf

prop_insert_preserves_bst :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Property
prop_insert_preserves_bst insertFunction integer = forAll searchTrees $ \tree -> isBST (insertFunction integer tree) == True

prop_insert_adds_element :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Property
-- prop_insert_adds_element insertFunction integer = forAll searchTrees $ \tree -> isInBST integer (insertFunction integer tree) == True
prop_insert_adds_element insertFunction integer = forAll searchTrees $ \tree -> (size tree) + 1 == size (insertFunction integer tree) && isInBST integer (insertFunction integer tree)

prop_insert_does_not_change_other_elements :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Integer -> Property
prop_insert_does_not_change_other_elements insertFunction integer newInteger = forAll searchTrees $ \tree -> isInBST integer (insertFunction newInteger (insertFunction integer tree)) == True

prop_insert_duplicate_check :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Property
prop_insert_duplicate_check insertFunction integer = forAll searchTrees $ \tree -> hasDuplicate integer (insertFunction integer (insertFunction integer tree)) == False

------------
prop_delete_detect_error :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Integer -> Property
prop_delete_detect_error deleteFunction i1 i2 = i1 /= i2 ==> deleteFunction i1 (Branch i2 Leaf Leaf) == Branch i2 Leaf Leaf

prop_delete_preserves_bst :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Property
prop_delete_preserves_bst deleteFunction integer = forAll searchTrees $ \tree -> isBST (deleteFunction integer tree) == True

prop_delete_removes_element :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Property
prop_delete_removes_element deleteFunction integer = forAll searchTrees $ \tree -> isInBST integer (deleteFunction integer tree) == False

prop_delete_does_not_change_other_elements :: (Integer -> BinaryTree -> BinaryTree) -> Integer -> Integer -> Property
prop_delete_does_not_change_other_elements deleteFunction integer newInteger = forAll searchTrees $ \tree -> isInBST integer (deleteFunction integer (deleteFunction newInteger tree)) == False

------------

height :: BinaryTree -> Integer
height Leaf = 0
height (Branch v l r) = 1 + (max (height l) (height r))

size :: BinaryTree -> Integer
size Leaf = 0
size (Branch v l r) = 1 + (size l) + (size r)
