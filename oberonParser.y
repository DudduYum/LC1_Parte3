{
module Main where
import Oberon
import OberonTools
}

%name newl
%tokentype { Token }
%error { parseError }

%token
  KW_INTEGER            { KW_TokenInteger }
  KW_REAL               { KW_TokenReal }
  KW_BOOLEAN            { KW_TokenBoolean }
  KW_CHAR               { KW_TokenChar }
  KW_SET                { KW_TokenSet }
  KW_ARRAY              { KW_TokenArray }
  KW_OF                 { KW_TokenOf }
  KW_POINTER_TO         { KW_TokenPointerTo }
  KW_PROCEDURE          { KW_TokenProcedure }
  KW_BEGIN              { KW_TokenBegin }
  KW_END                { KW_TokenEnd }
  KW_VAR                { KW_TokenVar }
  KW_CONST              { KW_TokenConst }
  KW_TRUE               { KW_TokenTrue }
  KW_FALSE              { KW_TokenFalse }
  KW_IF                 { KW_TokenIf }
  KW_ELSIF              { KW_TokenElsif }
  KW_ELSE               { KW_TokenElse }
  KW_THEN               { KW_TokenThen }
  KW_CASE               { KW_TokenCase }
  KW_WHILE              { KW_TokenWhile }
  KW_DO                 { KW_TokenDo }
  KW_REPEAT             { KW_TokenRepeat }
  KW_LOOP               { KW_TokenLoop }
  KW_EXIT               { KW_TokenExit }
  KW_RETURN             { KW_TokenReturn }
  KW_BREAK              { KW_TokenBreak }
  KW_CONTINUE           { KW_TokenContinue }
  KW_OR                 { KW_TokenOr }
  '&'                   { KW_TokenCommercialE }
  '~'                   { KW_TokenTilde }
  '+'                   { KW_TokenPlus }
  '-'                   { KW_TokenMinus }
  '*'                   { KW_TokenStar }
  '/'                   { KW_TokenForwardSlash }
  KW_DIV                { KW_TokenDiv }
  KW_MOD                { KW_TokenMod }
  '='                   { KW_TokenEquel }
  '#'                   { KW_TokenDiesis }
  '<'                   { KW_TokenMinor }
  '<='                  { KW_TokenMinorEqual }
  '>'                   { KW_TokenMajor }
  '<='                  { KW_TokenMajorEqual }
  ':='                  { KW_TokenAssignment }
  identifier 			{ TokenVariableIdentifier $$ }
  integerNum            { TokenIntegerNumber $$ }
  realNum 				{ TokenRealNumber $$ }
  validChar             { TokenValidChar $$ }
  validString        	{ TokenValidString $$ }

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