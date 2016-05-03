{
module Main (main) where
}

%wrapper "basic"

$identifier = [a-zA-Z] 		--

tokens :-


  $white+				;
  [\:\,]	;
  "INTEGER"     { \s -> TokInt }
  "REAL"        { \s -> TokReal }
  "BOOLEAN"     { \s -> TokBool }
  "SET"         { \s -> TokSet }
  "Function"    { \s -> TokFunc }
  $identifier [$identifier \_ \']*   { \s -> IdenTok s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	IdenTok String|
	TokInt  |
	TokReal |
	TokBool |
	TokSet  |
	TokFunc 
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
