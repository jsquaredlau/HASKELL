{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

module Ex05 where

data Tag where
  Empty :: Tag
  NonEmpty :: Tag
  HalfEmpty :: Tag

data BinaryTree (a :: Tag) b where
  Leaf   :: BinaryTree Empty b
  Branch :: b -> BinaryTree a b -> BinaryTree a b -> BinaryTree NonEmpty b
  HalfBranch :: b -> BinaryTree a b -> BinaryTree Empty b -> BinaryTree HalfEmpty b

top :: BinaryTree NonEmpty b -> b
top (Branch v l r) = v

----

data BoolProp (a :: Bool) where
  PTrue  :: BoolProp True
  PFalse :: BoolProp False
  PAnd   :: BoolProp a -> BoolProp b -> BoolProp (a && b)
  POr    :: BoolProp a -> BoolProp b -> BoolProp (a || b)
  PNot   :: BoolProp a -> BoolProp (Not a)

type family Not (a :: Bool) :: Bool
type instance Not True  = False
type instance Not False = True

type family (&&) (a :: Bool) (b :: Bool) :: Bool
type instance (&&) True True = True
type instance (&&) True False = False
type instance (&&) False True = False
type instance (&&) False False = False

type family (||) (a :: Bool) (b :: Bool) :: Bool
type instance (||) True True = True
type instance (||) True False = True
type instance (||) False True = True
type instance (||) False False = False

class ToRuntimeBool a where
  eval :: a -> Bool

instance ToRuntimeBool (BoolProp True) where
  eval _ = True

instance ToRuntimeBool (BoolProp False) where
  eval _ = False
