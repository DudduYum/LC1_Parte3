{
module Main (main) where
}

%wrapper "basic"

$identifier = [a-zA-Z] 		--
$digit = 0-9   -- numeri

tokens :-


  $white+				;
  [\:\,]	;
  "INTEGER"     { \s -> TokInt }
  "REAL"        { \s -> TokReal }
  "BOOLEAN"     { \s -> TokBool }
  "SET"         { \s -> TokSet }
  "Function"    { \s -> TokFunc }
  "Tree"        { \s -> TokTree}
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
	TokFunc |
  TokTree
	deriving (Eq,Show)

-- main = do
--    s <- getContents
--    print (alexScanTokens s)
}
