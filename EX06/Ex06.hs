{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

module Ex06 where

data List a = Nil | a ::: List a

data Format (fmt :: List *) where
  X :: Format Nil
  L :: String -> Format fmt -> Format fmt
  S :: Format fmt -> Format (String ::: fmt)
  I :: Format fmt -> Format (Int ::: fmt)

type family FormatArgsThen (fmt :: List *) (ty :: *)
type instance FormatArgsThen Nil ty = ty
type instance FormatArgsThen (t ::: fmt) ty = t -> FormatArgsThen fmt ty

printf :: Format fmt -> FormatArgsThen fmt String
printf f = printf' f ""
  where
    printf' :: Format fmt -> String -> FormatArgsThen fmt String
    printf' (X) s = s
    printf' (L str f) s = printf' f (s ++ str)
    printf' (S f) s = \str -> printf' f (s ++ str)
    printf' (I f) s = \i -> printf' f (s ++ (show i))
