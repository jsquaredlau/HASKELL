module Ex09 (BinaryTree(..), treeMap, concTreeMap) where

import Control.Concurrent
import System.Random
import Control.Applicative
import System.Environment

data BinaryTree a = Leaf | Branch a (BinaryTree a) (BinaryTree a) deriving (Read, Show, Eq)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Leaf = Leaf
treeMap f (Branch v l r) = let v' = f v
                               l' = treeMap f l
                               r' = treeMap f r
                           in
                           Branch v' l' r'

concTreeMap :: (a -> b) -> BinaryTree a -> IO (BinaryTree b)
concTreeMap f Leaf = return Leaf
concTreeMap f (Branch v l r) = do
                                  -- m1 <- newEmptyMVar
                                  -- m2 <- newEmptyMVar
                                  m3 <- newEmptyMVar
                                  -- forkIO $ do
                                  --   putMVar m1 (concTreeMap f l)
                                  -- forkIO $ do
                                  --   putMVar m2 (concTreeMap f r)
                                  forkIO $ putMVar m3 (f v)
                                  -- l' <- takeMVar m1
                                  -- r' <- takeMVar m2
                                  v' <- takeMVar m3
                                  -- l'' <- l'
                                  -- r'' <- r'
                                  -- v' `seq` l'' `seq` r'' `seq` (return (Branch v' l'' r''))
                                  l'' <- concTreeMap f l
                                  r'' <- concTreeMap f r
                                  v' `seq` l'' `seq` r'' `seq` (return (Branch v' l'' r''))


-- parEval :: (a -> b) -> a -> IO (ThreadId)
-- parEval f v = forkIO $  let
--                           v' = f v
--                         in
--                         do
--                           m <- newEmptyMVar
--                           v' `seq` return (putMVar m v')

-- concTreeMap :: (a -> b) -> BinaryTree a -> IO (BinaryTree b)
-- concTreeMap function Leaf = return Leaf
-- concTreeMap function (Branch v l r) = do let v' = (function v)
--                                          l' <- (concTreeMap function l)
--                                          r' <- (concTreeMap function r)
--                                          return (Branch v' l' r')
