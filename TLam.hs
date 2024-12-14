
module TLam where

import Data.List
import ParserLambda

type Gamma = [(Char, Int)]
data TermNL = VarNL Int 
            | AbsNL TermNL
            | AppNL TermNL TermNL deriving (Show, Eq )


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

eval (App t1 t2) = let t1' = eval t1 in App t1' t2 -- E-APP-1

-- funcao de shifting. c eh o cutoff
shifting :: TermNL -> Int -> Int -> TermNL
shifting (VarNL k) d c = if k<c
    then VarNL k
    else VarNL (k+d)
shifting (AbsNL t1) d c = AbsNL (shifting t1 d (c+1))
shifting (AppNL t1 t2) d c = AppNL (shifting t1 d c) (shifting t2 d c)

-- funcao igual ao subs soh q NL. adicao eh o shifting
-- nao libera nem captura variaveis
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

-- transforma as variaveis de um termo em indices de De Bruijn
removeNames :: Gamma -> Term -> TermNL
removeNames gamma (Var x) = case lookup x gamma of
    Just n  -> VarNL n
    Nothing -> error "Variable not found in context"
removeNames gamma (Abs x t1) = AbsNL (removeNames ((x, 0) : map (\(y, n) -> (y, n + 1)) gamma) t1)
removeNames gamma (App t1 t2) = AppNL (removeNames gamma t1) (removeNames gamma t2)

-- Restaura nomes de variáveis a partir de índices de De Bruijn
restoreNames :: Gamma -> TermNL -> Term
restoreNames gamma (VarNL n) = case lookup n (map swap gamma) of
    Just x  -> Var x
    Nothing -> error "Index not found in context"
  where
    swap (a, b) = (b, a)
restoreNames gamma (AbsNL t1) = Abs x' t1'
  where
    x' = fst (gamma !! (length gamma - 1)) -- Restaura o nome da variável ligada
    t1' = restoreNames ((x', length gamma - 1) : gamma) t1
restoreNames gamma (AppNL t1 t2) = App (restoreNames gamma t1) (restoreNames gamma t2)

-- funcao master que faz quase tudo
runEvalAndRestore :: Gamma -> Term -> Term
runEvalAndRestore gamma term =
  let termNL = removeNames gamma term        -- Remove nomes
      termNLEval = evalNL gamma termNL       -- Avalia sem nomes
  in restoreNames gamma termNLEval           -- Restaura os nomes

runTests :: IO ()
runTests = do
    let result1 = runEvalAndRestore [('x', 0), ('y', 1)] (Var 'x') -- Deve retornar Var 'x'
    if result1 == Var 'x' then putStrLn "Teste 1 passou" else putStrLn "Teste 1 falhou"

    let result2 = runEvalAndRestore [] (Abs 'x' (Var 'x')) -- Deve retornar Abs 'x' (Var 'x')
    print result2
    if result2 == Abs 'x' (Var 'x') then putStrLn "Teste 2 passou" else putStrLn "Teste 2 falhou"
