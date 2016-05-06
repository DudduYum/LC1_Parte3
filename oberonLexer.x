{
module OberonLexer where
}

%wrapper "monad"

$alpha      = [a-zA-Z]                  -- Lettere
$digit      = [0-9]                     -- Cifra
$validChar  = [^\"]                     -- Carattere in ascii tranne i doppi apici

tokens :-

  $white+				            ;
  -- Keywords
  "INTEGER"                 { mkL Lex_KW_TokenInteger }
  "REAL"                    { mkL Lex_KW_TokenReal }
  "BOOLEAN"                 { mkL Lex_KW_TokenBoolean }
  "CHAR"                    { mkL Lex_KW_TokenChar }
  "ARRAY"                   { mkL Lex_KW_TokenArray }
  "OF"                      { mkL Lex_KW_TokenOf }
  "POINTER TO"              { mkL Lex_KW_TokenPointerTo }
  "PROCEDURE"               { mkL Lex_KW_TokenProcedure }
  "BEGIN"                   { mkL Lex_KW_TokenBegin }
  "END"                     { mkL Lex_KW_TokenEnd }
  "VAR"                     { mkL Lex_KW_TokenVar }
  "CONST"                   { mkL Lex_KW_TokenConst }
  "TRUE"                    { mkL Lex_KW_TokenTrue }
  "FALSE"                   { mkL Lex_KW_TokenFalse }
  "IF"                      { mkL Lex_KW_TokenIf }
  "ELSIF"                   { mkL Lex_KW_TokenElsif }
  "ELSE"                    { mkL Lex_KW_TokenElse }
  "THEN"                    { mkL Lex_KW_TokenThen }
  "CASE"                    { mkL Lex_KW_TokenCase }
  "WHILE"                   { mkL Lex_KW_TokenWhile }
  "DO"                      { mkL Lex_KW_TokenDo }
  "REPEAT"                  { mkL Lex_KW_TokenRepeat }
  "UNTIL"                   { mkL Lex_KW_TokenUntil }
  "LOOP"                    { mkL Lex_KW_TokenLoop }
  "EXIT"                    { mkL Lex_KW_TokenExit }
  "RETURN"                  { mkL Lex_KW_TokenReturn }
  "BREAK"                   { mkL Lex_KW_TokenBreak }
  "CONTINUE"                { mkL Lex_KW_TokenContinue }

  -- Operators
  "OR"                      { mkL Lex_KW_TokenOr }
  "&"                       { mkL Lex_KW_TokenCommercialE }
  "~"                       { mkL Lex_KW_TokenTilde }
  "+"                       { mkL Lex_KW_TokenPlus }
  "-"                       { mkL Lex_KW_TokenMinus }
  "*"                       { mkL Lex_KW_TokenStar }
  "/"                       { mkL Lex_KW_TokenForwardSlash }
  "DIV"                     { mkL Lex_KW_TokenDiv }
  "MOD"                     { mkL Lex_KW_TokenMod }
  "="                       { mkL Lex_KW_TokenEqual }
  "#"                       { mkL Lex_KW_TokenDiesis }
  "<"                       { mkL Lex_KW_TokenMinor }
  "<="                      { mkL Lex_KW_TokenMinorEqual }
  ">"                       { mkL Lex_KW_TokenMajor }
  "<="                      { mkL Lex_KW_TokenMajorEqual }
  ":="                      { mkL Lex_KW_TokenAssignment }
  "."                       { mkL Lex_KW_TokenPoint }
  ","                       { mkL Lex_KW_TokenComa }
  ":"                       { mkL Lex_KW_TokenColon }
  ";"                       { mkL Lex_KW_TokenSemiColon }
  "("                       { mkL Lex_KW_TokenOpenBracket }
  ")"                       { mkL Lex_KW_TokenClosedBracket }
  "["                       { mkL Lex_KW_TokenOpenSquareBracket }
  "]"                       { mkL Lex_KW_TokenClosedSquareBracket }
  "|"                       { mkL Lex_KW_TokenPipe }
  \"                        { mkL Lex_KW_TokenDoubleQuotes }

  -- Variable/Constant/Procedure identifier
  $alpha [$alpha $digit \_]* { mkL Lex_TokenVariableIdentifier }

  -- Numeri e stringhe
  [1-9] $digit*             { mkL Lex_TokenIntegerNumber }
  [1-9] $digit* "." $digit+ { mkL Lex_TokenRealNumber }
  \" $validChar \"          { mkL Lex_TokenValidChar }
  \" $validChar* \"         { mkL Lex_TokenValidString }
{

data LexClass = 
  Lex_KW_TokenInteger             |
  Lex_KW_TokenReal                |
  Lex_KW_TokenBoolean             |
  Lex_KW_TokenChar                |
  Lex_KW_TokenArray               |
  Lex_KW_TokenOf                  |
  Lex_KW_TokenPointerTo           |
  Lex_KW_TokenProcedure           |
  Lex_KW_TokenBegin               |
  Lex_KW_TokenEnd                 |
  Lex_KW_TokenVar                 |
  Lex_KW_TokenConst               |
  Lex_KW_TokenTrue                |
  Lex_KW_TokenFalse               |
  Lex_KW_TokenIf                  |
  Lex_KW_TokenElsif               |
  Lex_KW_TokenElse                |
  Lex_KW_TokenThen                |
  Lex_KW_TokenCase                |
  Lex_KW_TokenWhile               |
  Lex_KW_TokenDo                  |
  Lex_KW_TokenRepeat              |
  Lex_KW_TokenUntil               |
  Lex_KW_TokenLoop                |
  Lex_KW_TokenExit                |
  Lex_KW_TokenReturn              |
  Lex_KW_TokenBreak               |
  Lex_KW_TokenContinue            |
  Lex_KW_TokenOr                  |
  Lex_KW_TokenCommercialE         |
  Lex_KW_TokenTilde               |
  Lex_KW_TokenPlus                |
  Lex_KW_TokenMinus               |
  Lex_KW_TokenStar                |
  Lex_KW_TokenForwardSlash        |
  Lex_KW_TokenDiv                 |
  Lex_KW_TokenMod                 |
  Lex_KW_TokenEqual               |
  Lex_KW_TokenDiesis              |
  Lex_KW_TokenMinor               |
  Lex_KW_TokenMinorEqual          |
  Lex_KW_TokenMajor               |
  Lex_KW_TokenMajorEqual          |
  Lex_KW_TokenAssignment          |
  Lex_KW_TokenPoint               |
  Lex_KW_TokenComa                |
  Lex_KW_TokenColon               |
  Lex_KW_TokenSemiColon           |
  Lex_KW_TokenOpenBracket         |
  Lex_KW_TokenClosedBracket       |
  Lex_KW_TokenOpenSquareBracket   |
  Lex_KW_TokenClosedSquareBracket |
  Lex_KW_TokenPipe                |
  Lex_KW_TokenDoubleQuotes        |
  Lex_TokenVariableIdentifier     |
  Lex_TokenIntegerNumber          |
  Lex_TokenRealNumber             |
  Lex_TokenValidChar              |
  Lex_TokenValidString            |
  Lex_EOF
  deriving (Show, Eq)

mkL :: LexClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len =  let t = take len str
                            in case c of  Lex_KW_TokenInteger             -> return (KW_TokenInteger p)
                                          Lex_KW_TokenReal                -> return (KW_TokenReal p)
                                          Lex_KW_TokenBoolean             -> return (KW_TokenBoolean p)
                                          Lex_KW_TokenChar                -> return (KW_TokenChar p)
                                          Lex_KW_TokenArray               -> return (KW_TokenArray p)
                                          Lex_KW_TokenOf                  -> return (KW_TokenOf p)
                                          Lex_KW_TokenPointerTo           -> return (KW_TokenPointerTo p)
                                          Lex_KW_TokenProcedure           -> return (KW_TokenProcedure p)
                                          Lex_KW_TokenBegin               -> return (KW_TokenBegin p)
                                          Lex_KW_TokenEnd                 -> return (KW_TokenEnd p)
                                          Lex_KW_TokenVar                 -> return (KW_TokenVar p)
                                          Lex_KW_TokenConst               -> return (KW_TokenConst p)
                                          Lex_KW_TokenTrue                -> return (KW_TokenTrue p)
                                          Lex_KW_TokenFalse               -> return (KW_TokenFalse p)
                                          Lex_KW_TokenIf                  -> return (KW_TokenIf p)
                                          Lex_KW_TokenElsif               -> return (KW_TokenElsif p)
                                          Lex_KW_TokenElse                -> return (KW_TokenElse p)
                                          Lex_KW_TokenThen                -> return (KW_TokenThen p)
                                          Lex_KW_TokenCase                -> return (KW_TokenCase p)
                                          Lex_KW_TokenWhile               -> return (KW_TokenWhile p)
                                          Lex_KW_TokenDo                  -> return (KW_TokenDo p)
                                          Lex_KW_TokenRepeat              -> return (KW_TokenRepeat p)
                                          Lex_KW_TokenUntil               -> return (KW_TokenUntil p)
                                          Lex_KW_TokenLoop                -> return (KW_TokenLoop p)
                                          Lex_KW_TokenExit                -> return (KW_TokenExit p)
                                          Lex_KW_TokenReturn              -> return (KW_TokenReturn p)
                                          Lex_KW_TokenBreak               -> return (KW_TokenBreak p)
                                          Lex_KW_TokenContinue            -> return (KW_TokenContinue p)
                                          Lex_KW_TokenOr                  -> return (KW_TokenOr p)
                                          Lex_KW_TokenCommercialE         -> return (KW_TokenCommercialE p)
                                          Lex_KW_TokenTilde               -> return (KW_TokenTilde p)
                                          Lex_KW_TokenPlus                -> return (KW_TokenPlus p)
                                          Lex_KW_TokenMinus               -> return (KW_TokenMinus p)
                                          Lex_KW_TokenStar                -> return (KW_TokenStar p)
                                          Lex_KW_TokenForwardSlash        -> return (KW_TokenForwardSlash p)
                                          Lex_KW_TokenDiv                 -> return (KW_TokenDiv p)
                                          Lex_KW_TokenMod                 -> return (KW_TokenMod p)
                                          Lex_KW_TokenEqual               -> return (KW_TokenEqual p)
                                          Lex_KW_TokenDiesis              -> return (KW_TokenDiesis p)
                                          Lex_KW_TokenMinor               -> return (KW_TokenMinor p)
                                          Lex_KW_TokenMinorEqual          -> return (KW_TokenMinorEqual p)
                                          Lex_KW_TokenMajor               -> return (KW_TokenMajor p)
                                          Lex_KW_TokenMajorEqual          -> return (KW_TokenMajorEqual p)
                                          Lex_KW_TokenAssignment          -> return (KW_TokenAssignment p)
                                          Lex_KW_TokenPoint               -> return (KW_TokenPoint p)
                                          Lex_KW_TokenComa                -> return (KW_TokenComa p)
                                          Lex_KW_TokenColon               -> return (KW_TokenColon p)
                                          Lex_KW_TokenSemiColon           -> return (KW_TokenSemiColon p)
                                          Lex_KW_TokenOpenBracket         -> return (KW_TokenOpenBracket p)
                                          Lex_KW_TokenClosedBracket       -> return (KW_TokenClosedBracket p)
                                          Lex_KW_TokenOpenSquareBracket   -> return (KW_TokenOpenSquareBracket p)
                                          Lex_KW_TokenClosedSquareBracket -> return (KW_TokenClosedSquareBracket p)
                                          Lex_KW_TokenPipe                -> return (KW_TokenPipe p)
                                          Lex_KW_TokenDoubleQuotes        -> return (KW_TokenDoubleQuotes p)
                                          Lex_TokenVariableIdentifier     -> return (TokenVariableIdentifier t p)
                                          Lex_TokenIntegerNumber          -> return (TokenIntegerNumber ((read t) :: Integer) p)
                                          Lex_TokenRealNumber             -> return (TokenRealNumber ((read t) :: Float) p)
                                          Lex_TokenValidChar              -> return (TokenValidChar (head (tail t)) p)
                                          Lex_TokenValidString            -> return (TokenValidString (init (tail t)) p)
                                          Lex_EOF                         -> return (EOF)

alexEOF :: Alex Token
alexEOF = return EOF

-- The token type:
data Token =
  KW_TokenInteger                 { position :: AlexPosn } |
  KW_TokenReal                    { position :: AlexPosn } |
  KW_TokenBoolean                 { position :: AlexPosn } |
  KW_TokenChar                    { position :: AlexPosn } |
  KW_TokenSet                     { position :: AlexPosn } |
  KW_TokenArray                   { position :: AlexPosn } |
  KW_TokenOf                      { position :: AlexPosn } |
  KW_TokenPointerTo               { position :: AlexPosn } |
  KW_TokenProcedure               { position :: AlexPosn } |
  KW_TokenBegin                   { position :: AlexPosn } |
  KW_TokenEnd                     { position :: AlexPosn } |
  KW_TokenVar                     { position :: AlexPosn } |
  KW_TokenConst                   { position :: AlexPosn } |
  KW_TokenTrue                    { position :: AlexPosn } |
  KW_TokenFalse                   { position :: AlexPosn } |
  KW_TokenIf                      { position :: AlexPosn } |
  KW_TokenElsif                   { position :: AlexPosn } |
  KW_TokenElse                    { position :: AlexPosn } |
  KW_TokenThen                    { position :: AlexPosn } |
  KW_TokenCase                    { position :: AlexPosn } |
  KW_TokenWhile                   { position :: AlexPosn } |
  KW_TokenDo                      { position :: AlexPosn } |
  KW_TokenRepeat                  { position :: AlexPosn } |
  KW_TokenUntil                   { position :: AlexPosn } |
  KW_TokenLoop                    { position :: AlexPosn } |
  KW_TokenExit                    { position :: AlexPosn } |
  KW_TokenReturn                  { position :: AlexPosn } |
  KW_TokenBreak                   { position :: AlexPosn } |
  KW_TokenContinue                { position :: AlexPosn } |
  KW_TokenOr                      { position :: AlexPosn } |
  KW_TokenCommercialE             { position :: AlexPosn } |
  KW_TokenTilde                   { position :: AlexPosn } |
  KW_TokenPlus                    { position :: AlexPosn } |
  KW_TokenMinus                   { position :: AlexPosn } |
  KW_TokenStar                    { position :: AlexPosn } |
  KW_TokenForwardSlash            { position :: AlexPosn } |
  KW_TokenDiv                     { position :: AlexPosn } |
  KW_TokenMod                     { position :: AlexPosn } |
  KW_TokenEqual                   { position :: AlexPosn } |
  KW_TokenDiesis                  { position :: AlexPosn } |
  KW_TokenMinor                   { position :: AlexPosn } |
  KW_TokenMinorEqual              { position :: AlexPosn } |
  KW_TokenMajor                   { position :: AlexPosn } |
  KW_TokenMajorEqual              { position :: AlexPosn } |
  KW_TokenAssignment              { position :: AlexPosn } |
  KW_TokenPoint                   { position :: AlexPosn } |
  KW_TokenComa                    { position :: AlexPosn } |
  KW_TokenColon                   { position :: AlexPosn } |
  KW_TokenSemiColon               { position :: AlexPosn } |
  KW_TokenOpenBracket             { position :: AlexPosn } |
  KW_TokenClosedBracket           { position :: AlexPosn } |
  KW_TokenOpenSquareBracket       { position :: AlexPosn } |
  KW_TokenClosedSquareBracket     { position :: AlexPosn } |
  KW_TokenPipe                    { position :: AlexPosn } |
  KW_TokenDoubleQuotes            { position :: AlexPosn } |
  TokenVariableIdentifier         { name    :: String,  position :: AlexPosn } |
  TokenIntegerNumber              { intVal  :: Integer, position :: AlexPosn } |
  TokenRealNumber                 { fltVal  :: Float,   position :: AlexPosn } |
  TokenValidChar                  { chrVal  :: Char,    position :: AlexPosn } |
  TokenValidString                { strVal  :: String,  position :: AlexPosn } |
  EOF
	deriving (Eq,Show)
}