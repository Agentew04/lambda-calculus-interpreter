
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
subs x s (Abs y t) = Abs y (subs x s t)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2) 

-- ainda falta transformar em call by value
-- agora eh um full beta reduction
eval :: Term -> Term
eval (Var x) = Var x
eval (Abs x t) = Abs x (eval t)
eval (App (Abs x t1) t2) = eval (subs x t2 t1)
eval (App t1 t2) = App (eval t1) (eval t2)
