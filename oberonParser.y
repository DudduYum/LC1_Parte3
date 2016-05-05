{
module Main where
import OberonLexer
import OberonTools
}

%name oLikeParse
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
  KW_UNTIL             	{ KW_TokenUntil }
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
  KW_MinorEqual         { KW_TokenMinorEqual }
  '>'                   { KW_TokenMajor }
  KW_MajorEqual         { KW_TokenMajorEqual }
  KW_Assignment         { KW_TokenAssignment }
  '.'					          { KW_TokenPoint }
  ','         					{ KW_TokenComa }
  ':'					          { KW_TokenColon }
  ';'         					{ KW_TokenSemiColon }
  '('         					{ KW_TokenOpenBracket }
  ')'         					{ KW_TokenClosedBracket }
  '['         					{ KW_TokenOpenSquareBracket }
  ']'         					{ KW_TokenClosedSquareBracket }
  identifier 			      { TokenVariableIdentifier $$ }
  integerNum            { TokenIntegerNumber $$ }
  realNum 				      { TokenRealNumber $$ }
  validChar             { TokenValidChar $$ }
  validString        	  { TokenValidString $$ }

%%

ProcedureDeclarationList  :   ProcedureDeclaration                              { [$1] }
                          |   ProcedureDeclaration ';' ProcedureDeclarationList { $1:$3 }
                          -- aggiunta temporale


ProcedureDeclaration  : ProcedureHeading ';' ProcedureBody identifier {
                                                                        do
                                                                          let newProc = $1
                                                                          if (procedureName newProc) == $4 then
                                                                            defaultDeclaration { declarationType = DT_Procedure, procedureDeclared = Just (addBodyToProcedure newProc $3) }
                                                                          else
                                                                            parseError [KW_TokenChar]
                                                                      }

IdentifiersList 		: 	identifier							        { [$1] }
						        |	  identifier ',' IdentifiersList  { $1:$3 }

VariableDeclaration : IdentifiersList ':' type          { createVariablesDefinitionsOfType $1 $3 }

ProcedureHeading    : KW_PROCEDURE identifier { defaultProcedure { procedureName = $2 } }
--            |   KW_PROCEDURE identifier FormalParameters


ProcedureBody     : KW_END                                      { [] }
                  | DeclarationSequenceList KW_END                  { $1 }
--            | DeclarationSequence KW_BEGIN StatementSequence KW_END   { }

DeclarationSequence   : KW_VAR VariableDeclarationList       { $2 }
--                      | KW_CONST ConstDeclarationList ';'       { $1 }
                      | ProcedureDeclarationList                { $1 }

DeclarationSequenceList : DeclarationSequence                         { $1 }
                        | DeclarationSequence DeclarationSequenceList { $1++$2 }

--ConstDeclarationList  : ConstDeclaration                            { $1 }
--                      | ConstDeclaration ';' ConstDeclarationList   { $1 $3 }

VariableDeclarationList : VariableDeclaration ';'                         { $1 }
                        | VariableDeclaration ';' VariableDeclarationList { $1++$3 }

--baseTypes				:	KW_INTEGER
--						|	KW_REAL
--						|	KW_BOOLEAN
--						| KW_POINTER_TO

type 				: 	KW_INTEGER                        { Simple Integer }
						|	  KW_REAL                           { Simple Float }
						|	  KW_BOOLEAN                        { Simple Boolean }
            |   KW_CHAR                           { Simple Char }
						|   KW_ARRAY lenghtList KW_OF type    { 
                                                    do
                                                      let lenList = $2
                                                      if listElementIsLessOrEqualZero lenList then
                                                        parseError [KW_TokenChar]
                                                      else
                                                        Array lenList $4
                                                  }
--						|	PointerType
--						|	ProcedureType

-- ArrayType				:  KW_ARRAY lenghtList KW_OF type      { Array $2 $4 } OLD

lenghtList 				: 	lenght                  { [$1] }
						      |	  lenght ',' lenghtList   { $1:$3 }

lenght					:	ConstExpression      { $1 }

--PointerType				: 	KW_POINTER_TO type

--ProcedureType 			:	KW_PROCEDURE
--						|	KW_PROCEDURE FormalParameters

--ConstantDeclaration		: 	identifier '=' ConstExpression

ConstExpression			: 	expression     { $1 }

--designator				:	identifier
--						|	identifier designatorHelper

--designatorHelper		: 	'.' designator
--						|	'[' ExpList ']'
--						|	'[' ExpList ']' designatorHelper

--ExpList					: 	expression
--						|	expression ',' ExpList

expression 				: 	SimpleExpression      { $1 }
--						| 	SimpleExpression relation SimpleExpression

--relation				: 	'='
--						| 	'#'
--						| 	'<'
--						| 	KW_MinorEqual
--						| 	'>'
--						| 	KW_MajorEqual

SimpleExpression		:	term        { $1  }
--						|	term AddOperatorList
						        |	'+' term    { $2  }
						        |	'-' term    { -$2 }
--						|	'+' term AddOperatorList
--						|	'-' term AddOperatorList

--AddOperator				:	'+'          {}
--						|	'-'
--						|	KW_OR

--AddOperatorList			:	AddOperator term
--						|	AddOperator term AddOperatorList

term 					:	factor        { $1 }
--						|	factor MulOperatorList

--MulOperator 			:	'*'
--						|	'/'
--						|	KW_DIV
--						| 	KW_MOD
--						|	'&'

--MulOperatorList			:	MulOperator factor
--						|	MulOperator factor MulOperatorList

factor	 				:	integerNum          { $1 }
--						|	realNum
--						|	'"' validChar '"'
--						|	'"' validString '"'
--						|	designator
--						|	designator ActualParameters
--						|	'(' expression ')'
--						|	'~' factor

--ActualParameters		: 	'(' ')'
--						|	'(' ExpList ')'

--statement 				:	assignment
--						|	ProcedureCall
--						|	IfStatement
--						|	CaseStatement
--						| WhileStatement
--						|	RepeatStatement
--						|	LoopStatement
--						|	KW_EXIT
--						|	KW_RETURN
--						|	KW_RETURN expression

--assignment 				:	designator KW_Assignment expression

--ProcedureCall 			:	designator
--						|	designator ActualParameters

--StatementSequence 		:	statement
--						|	statement ';' StatementSequence

--IfStatement 			:	KW_IF expression KW_THEN StatementSequence KW_END
--						|	KW_IF expression KW_THEN StatementSequence KW_ELSE StatementSequence KW_END
--						|	KW_IF expression KW_THEN StatementSequence ElseIfList KW_END
--						|	KW_IF expression KW_THEN StatementSequence ElseIfList KW_ELSE StatementSequence KW_END

--ElseIfList 				:	KW_ELSIF expression KW_THEN StatementSequence
--						|	KW_ELSIF expression KW_THEN StatementSequence ElseIfList

--CaseStatement 			: 	KW_CASE expression KW_OF Case KW_END
--						|	KW_CASE expression KW_OF Case KW_ELSE StatementSequence KW_END
--						|	KW_CASE expression KW_OF CaseList KW_END
--						|	KW_CASE expression KW_OF CaseList KW_ELSE StatementSequence KW_END

--Case 					:	CaseLabelList ':' StatementSequence

--CaseLabelList 			:	CaseLabels
--						|	CaseLabels ',' CaseLabelList

--CaseLabels 				:	ConstExpression
--						|	ConstExpression '..' ConstExpression

--WhileStatement 			:	KW_WHILE expression KW_DO StatementSequence KW_END

--RepeatStatement			:	KW_REPEAT StatementSequence KW_UNTIL expression

--LoopStatement 			:	KW_LOOP StatementSequence KW_END

--FormalParameters 		:	'(' ')'
--						|	'(' FPSectionList ')'
--						|	'(' FPSectionList ')' ':' type

--FPSection 				:	IdentifiersList ':' FormalType
--						|	KW_VAR IdentifiersList ':' FormalType

--FPSectionList 			: 	FPSection
--						|	FPSection ';' FPSectionList

--FormalType 				: 	baseTypes
--						|	KW_ARRAY KW_OF baseTypes

{
parseError :: [Token] -> a
parseError [] = error "Missing expected token"
parseError tk = error ("Unexpected token: " ++ (show (head tk)))

dStack = []

main = do
  inStr <- getContents
  --print (alexScanTokens inStr)
  let result = oLikeParse (alexScanTokens inStr)
  --oLikeParse (alexScanTokens inStr)
  putStrLn ("result: " ++ show(result))
  putStrLn("DONE")
}
