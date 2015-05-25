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
satisfiable (Body t)        = eval t
satisfiable (Forall xs f)   = any (satisfiable . f. Con) xs

solutions :: Formula ts -> [ts]
solutions (Body t)              = [() | eval t]
solutions (Forall inputs f)     = [(x, y) | x <- inputs, y <- solutions (f (Con x))]

-- takes element from outer most argument list,
-- recurse down, pick up elemnt from inner most list

-- Unwrap the formula as many instanes of lambda there are
-- at the lowest level find solutions for all inputs

-- 2 layers : for each element in outmost argument list, try solve formula with all arguments in second argument list
-- if that comes out with true then show arguments
