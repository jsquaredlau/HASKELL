module Ex08 (BinaryTree(..), treeMap, parTreeMap, parTreeMapWithCutoff) where

import System.Random 
import Control.Applicative
import Control.Parallel
import System.Environment

data BinaryTree a = Leaf | Branch a (BinaryTree a) (BinaryTree a) deriving (Read, Show, Eq)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Leaf = Leaf
treeMap f (Branch v l r) = let v' = f v
                               l' = treeMap f l 
                               r' = treeMap f r
                           in
                           v' `pseq` l' `pseq` r' `pseq` Branch v' l' r'

parTreeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
parTreeMap = error "IMPLEMENT THIS"

parTreeMapWithCutoff :: Int -> (a -> b) -> BinaryTree a -> BinaryTree b
parTreeMapWithCutoff = error "IMPLEMENT THIS"
