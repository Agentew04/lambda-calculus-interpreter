
module TLam where

import Data.List
import ParserLambda


freeVars :: Term -> [Char]
freeVars (Var x)     = [x] 
freeVars (Abs x t)   = delete x (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2


-- [x |-> s] y (x!=y)=> y
-- [x |-> s] x => s
-- [x |-> s] lambda y.t => lambda y.([x |-> s] t)
-- [x |-> s] (t1 t2) => ([x |-> s] t1) ([x |-> s] t2)
subs :: Char -> Term -> Term -> Term
subs x s (Var z) = if x == z then s else Var z
subs x s (Abs y t1) = if x==y then Abs y t1 else Abs y (subs x s t1)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2) 

isVal :: Term -> Bool
isVal (Var x) = True
isVal (Abs x t) = True
isVal _ = False

-- transformada em call by value
-- soh aplica subst quando 
eval :: Term -> Term
eval (Var x) = Var x
eval (Abs x t) = Abs x t
eval (App (Abs x t1) t2) = if isVal t2
    then subs x t2 t1 -- E-APP-ABS
    else let t2' = eval t2 in App t1 t2' -- E-APP-2
eval (App t1 t2) = let t1' = eval t1 in App t1' t2
