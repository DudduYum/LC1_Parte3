{
module OberonLexer where
}

%wrapper "basic"

$alpha      = [a-zA-Z]                  -- Lettere
$digit      = [0-9]                     -- Cifra
$validChar  = [^\"]                     -- Carattere in ascii tranne i doppi apici

tokens :-

  $white+				            ;
  -- Keywords
  "INTEGER"                 { \s -> KW_TokenInteger }
  "REAL"                    { \s -> KW_TokenReal }
  "BOOLEAN"                 { \s -> KW_TokenBoolean }
  "CHAR"                    { \s -> KW_TokenChar }
  "SET"                     { \s -> KW_TokenSet }
  "ARRAY"                   { \s -> KW_TokenArray }
  "OF"                      { \s -> KW_TokenOf }
  "POINTER TO"              { \s -> KW_TokenPointerTo }
  "PROCEDURE"               { \s -> KW_TokenProcedure }
  "BEGIN"                   { \s -> KW_TokenBegin }
  "END"                     { \s -> KW_TokenEnd }
  "VAR"                     { \s -> KW_TokenVar }
  "CONST"                   { \s -> KW_TokenConst }
  "TRUE"                    { \s -> KW_TokenTrue }
  "FALSE"                   { \s -> KW_TokenFalse }
  "IF"                      { \s -> KW_TokenIf }
  "ELSIF"                   { \s -> KW_TokenElsif }
  "ELSE"                    { \s -> KW_TokenElse }
  "THEN"                    { \s -> KW_TokenThen }
  "CASE"                    { \s -> KW_TokenCase }
  "WHILE"                   { \s -> KW_TokenWhile }
  "DO"                      { \s -> KW_TokenDo }
  "REPEAT"                  { \s -> KW_TokenRepeat }
  "UNTIL"                   { \s -> KW_TokenUntil }
  "LOOP"                    { \s -> KW_TokenLoop }
  "EXIT"                    { \s -> KW_TokenExit }
  "RETURN"                  { \s -> KW_TokenReturn }
  "BREAK"                   { \s -> KW_TokenBreak }
  "CONTINUE"                { \s -> KW_TokenContinue }


  -- Operators
  "OR"                      { \s -> KW_TokenOr }
  "&"                       { \s -> KW_TokenCommercialE }
  "~"                       { \s -> KW_TokenTilde }
  "+"                       { \s -> KW_TokenPlus }
  "-"                       { \s -> KW_TokenMinus }
  "*"                       { \s -> KW_TokenStar }
  "/"                       { \s -> KW_TokenForwardSlash }
  "DIV"                     { \s -> KW_TokenDiv }
  "MOD"                     { \s -> KW_TokenMod }
  "="                       { \s -> KW_TokenEquel }
  "#"                       { \s -> KW_TokenDiesis }
  "<"                       { \s -> KW_TokenMinor }
  "<="                      { \s -> KW_TokenMinorEqual }
  ">"                       { \s -> KW_TokenMajor }
  "<="                      { \s -> KW_TokenMajorEqual }
  ":="                      { \s -> KW_TokenAssignment }
  "."                       { \s -> KW_TokenPoint }
  ","                       { \s -> KW_TokenComa }
  ":"                       { \s -> KW_TokenColon }
  ";"                       { \s -> KW_TokenSemiColon }
  "("                       { \s -> KW_TokenOpenBracket }
  ")"                       { \s -> KW_TokenClosedBracket }
  "["                       { \s -> KW_TokenOpenSquareBracket }
  "]"                       { \s -> KW_TokenClosedSquareBracket }
  \"                        { \s -> KW_TokenDoubleQuotes }

  -- Variable/Constant/Procedure identifier
  $alpha [$alpha $digit \_]* { \s -> TokenVariableIdentifier s }

  -- Numeri e stringhe
  [1-9] $digit*             { \s -> TokenIntegerNumber (read s) }
  [1-9] $digit* "." $digit+ { \s -> TokenRealNumber (read s) }
  \" $validChar \"          { \s -> TokenValidChar (head s) }
  \" $validChar* \"         { \s -> TokenValidString s }
{

-- The token type:
data Token =
  KW_TokenInteger                 |
  KW_TokenReal                    |
  KW_TokenBoolean                 |
  KW_TokenChar                    |
  KW_TokenSet                     |
  KW_TokenArray                   |
  KW_TokenOf                      |
  KW_TokenPointerTo               |
  KW_TokenProcedure               |
  KW_TokenBegin                   |
  KW_TokenEnd                     |
  KW_TokenVar                     |
  KW_TokenConst                   |
  KW_TokenTrue                    |
  KW_TokenFalse                   |
  KW_TokenIf                      |
  KW_TokenElsif                   |
  KW_TokenElse                    |
  KW_TokenThen                    |
  KW_TokenCase                    |
  KW_TokenWhile                   |
  KW_TokenDo                      |
  KW_TokenRepeat                  |
  KW_TokenUntil                   |
  KW_TokenLoop                    |
  KW_TokenExit                    |
  KW_TokenReturn                  |
  KW_TokenBreak                   |
  KW_TokenContinue                |
  KW_TokenOr                      |
  KW_TokenCommercialE             |
  KW_TokenTilde                   |
  KW_TokenPlus                    |
  KW_TokenMinus                   |
  KW_TokenStar                    |
  KW_TokenForwardSlash            |
  KW_TokenDiv                     |
  KW_TokenMod                     |
  KW_TokenEquel                   |
  KW_TokenDiesis                  |
  KW_TokenMinor                   |
  KW_TokenMinorEqual              |
  KW_TokenMajor                   |
  KW_TokenMajorEqual              |
  KW_TokenAssignment              |
  KW_TokenPoint                   |
  KW_TokenComa                    |
  KW_TokenColon                   |
  KW_TokenSemiColon               |
  KW_TokenOpenBracket             |
  KW_TokenClosedBracket           |
  KW_TokenOpenSquareBracket       |
  KW_TokenClosedSquareBracket     |
  KW_TokenDoubleQuotes            |
  TokenVariableIdentifier String  |
  TokenIntegerNumber Integer      |
  TokenRealNumber Float           |
  TokenValidChar Char             |
  TokenValidString String
	deriving (Eq,Show)

}
