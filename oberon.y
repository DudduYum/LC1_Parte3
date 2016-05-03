{
module Grammar where
import oberonTok
}

%name parseCalc
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

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
