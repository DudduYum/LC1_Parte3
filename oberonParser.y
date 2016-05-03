{
module Main where
import Oberon
import OberonTools
}

%name newl
%tokentype { Token }
%error { parseError }

%token
    inden { IdenTok }
    inden { TokInt }
    inden { TokReal }
    inden { TokBool }
    inden { TokSet }
    inden { TokFunc }
    inden { TokTree }

-- %right in
-- %nonassoc '>' '<'
-- %left '+' '-'
-- %left '*' '/'
-- %left NEG

%%

{
parseError :: [Token] -> a
parseError _ = error "Parse errore"

main = do
	inStr <- getContents
	let result = newl (alexScanTokens inStr)
}