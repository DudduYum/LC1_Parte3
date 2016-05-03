{
module Main (main) where
}

%wrapper "basic"

-- $digit = 0-9			-- digits
-- $alpha = [a-zA-Z]		-- alphabetic characters
$identifier = [a-zA-Z] 		-- 
tokens :-


  $white+				;
  $identifier 				{ \s -> IdenTok s} 
  "INTEGER"				{ \s -> TokInt s}
  "REAL"                                { \s -> TokReal s}
  "BOOLEAN"                             { \s -> TokBool s}
  "SET"                                 { \s -> TokSet s}
  "Function"                            { \s -> TokFunc s}
  [\:\,]				;
  -- "--".*				;
  -- let					{ \s -> Let }
  -- in					{ \s -> In }
  -- $digit+				{ \s -> Int (read s) }
  -- [\=\+\-\*\/\(\)]			{ \s -> Sym (head s) }
  -- $alpha [$alpha $digit \_ \']*		{ \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	IdenTok ;
	TokInt  ; 
	TokReal ;
	TokBool ;
	TokSet  ;
	TokFunc ;
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
