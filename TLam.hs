module TLam where

import Data.List ( delete )
import ParserLambda
import System.IO (readFile)
import Data.Text.Array (run)

type Gamma = [(Char, Int)]
data TermNL = VarNL Int 
            | AbsNL TermNL
            | AppNL TermNL TermNL deriving (Show, Eq )

freeVars :: Term -> [Char]
freeVars (Var x)     = [x] 
freeVars (Abs x t)   = delete x (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

subs :: Char -> Term -> Term -> Term
subs x s (Var z) = if x == z then s else Var z
subs x s (Abs y t1) = if x==y then Abs y t1 else Abs y (subs x s t1)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2) 

isVal :: Term -> Bool
isVal (Var x) = True
isVal (Abs x t) = True
isVal _ = False

eval :: Term -> Term
eval (Var x) = Var x
eval (Abs x t) = Abs x t
eval (App (Abs x t1) t2) = if isVal t2
    then subs x t2 t1
    else let t2' = eval t2 in App t1 t2'
eval (App t1 t2) = let t1' = eval t1 in App t1' t2

shifting :: TermNL -> Int -> Int -> TermNL
shifting (VarNL k) d c = if k<c
    then VarNL k
    else VarNL (k+d)
shifting (AbsNL t1) d c = AbsNL (shifting t1 d (c+1))
shifting (AppNL t1 t2) d c = AppNL (shifting t1 d c) (shifting t2 d c)

subsNL :: Int -> TermNL -> TermNL -> TermNL
subsNL j s (VarNL k) = if j==k then s else VarNL k
subsNL j s (AbsNL t1) = AbsNL (subsNL (j+1) (shifting s 1 0) t1)
subsNL j s (AppNL t1 t2) = AppNL (subsNL j s t1) (subsNL j s t2)

evalNL :: Gamma -> TermNL -> TermNL
evalNL gamma (VarNL n) = VarNL n
evalNL gamma (AbsNL t1) = AbsNL (evalNL gamma t1)
evalNL gamma (AppNL t1 t2) = case evalNL gamma t1 of
    AbsNL t1' -> evalNL gamma (subsNL 0 t2 t1')
    t1' -> AppNL t1' t2

removeNames :: Gamma -> Term -> TermNL
removeNames gamma (Var x) = case lookup x gamma of
    Just n  -> VarNL n
    Nothing -> error $ "Variable " ++ [x] ++ " not found in context"
removeNames gamma (Abs x t1) = AbsNL (removeNames ((x, 0) : map (\(y, n) -> (y, n + 1)) gamma) t1)
removeNames gamma (App t1 t2) = AppNL (removeNames gamma t1) (removeNames gamma t2)

restoreNames :: Gamma -> TermNL -> Term
restoreNames gamma (VarNL n) = case lookup n (map swap gamma) of
    Just x  -> Var x
    Nothing -> error "Index not found in context"
  where
    swap (a, b) = (b, a)
restoreNames gamma (AbsNL t1) = Abs x' t1'
  where
    x' = fst (gamma !! (length gamma - 1))
    t1' = restoreNames ((x', length gamma - 1) : gamma) t1
restoreNames gamma (AppNL t1 t2) = App (restoreNames gamma t1) (restoreNames gamma t2)

runEvalAndRestore :: Term -> Term
runEvalAndRestore term = 
  let freeVarsList = freeVars term
      gamma = if null freeVarsList then [('y', 0)] else zip freeVarsList [0..]  -- Cria o contexto com entrada coringa se necessário
      termNL = removeNames gamma term        -- Remove nomes
      termNLEval = evalNL gamma termNL       -- Avalia sem nomes
  in restoreNames gamma termNLEval           -- Restaura os nomes

main = readFile "input" >>= print . runEvalAndRestore . parserlamb . lexer
