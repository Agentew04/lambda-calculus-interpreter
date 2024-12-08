{
module ParserLambda where
import Data.Char
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam } 
	var { TokenVar $$ }
	'.' { TokenPoint }
	'(' { TokenOB }
	')' { TokenCB }

%left APP	
%right '.'

%%

-- regras de producao da gramatica


-- ToDo: fazer a correspondencia com a nossa sintaxe abstrata
Term : var                   { Var $1 }
	 | lam var  '.' Term     { Abs $2 $4}
	 | Term Term  %prec APP  { App $1 $2 }
	 | '(' Term ')'          { $2 }


{

parseError :: [Token] -> a
parseError b = error "Parse Error"

-- Def da sintaxe abstrata: que está diferente da definida em 
-- Lam.hs! 
data Term = Var Char
           | Abs Char Term
           | App Term Term deriving (Show, Eq)


data Token 
		= TokenVar Char
		| TokenPoint
		| TokenOB
		| TokenCB
		| TokenLam 
	deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer s@(c:cs)
    | isSpace c = lexer cs  --whitespace
    | take 3 s == "lam" = TokenLam : lexer (drop 3 s)  -- lamda aka "lam"
    | c == '.'  = TokenPoint : lexer cs  -- dot
    | c == '('  = TokenOB : lexer cs
    | c == ')'  = TokenCB : lexer cs
    | isAlpha c = TokenVar c : lexer cs  -- vars
    | otherwise = error $ "Lexer error: caractere inválido " ++ [c]



main = getContents >>= print . parserlamb .lexer

}
