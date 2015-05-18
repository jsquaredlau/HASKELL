{-# LANGUAGE GADTs #-}

module Solver where

import Formula


-- Evaluating terms
-- ----------------

eval :: Term t -> t
eval (Con t)        = t
eval (And x y)      = eval x && eval y
eval (Or x y)       = eval x || eval y
eval (Smaller x y)  = eval x < eval y
eval (Plus x y)     = eval x + eval y


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body t) = eval t
satisfiable (Forall xs f) = any (satisfiable . f. Con) xs

solutions :: Formula ts -> [ts]
solutions (Body term) = [() | eval term]
solutions (Forall xs formula) = [(x, ys) | x <- xs, ys <- solutions(formula(Con x))]
