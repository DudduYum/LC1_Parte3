{
module Main where
import Oberon
import OberonTools

let depth = 0
let proceduresStack = []
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

IdentifiersList 		: 	identifier
						|	identifier ',' IdentifiersList

baseTypes				:	KW_INTEGER
						|	KW_REAL
						|	KW_BOOLEAN
						| 	KW_POINTER_TO

type 					: 	KW_INTEGER
						|	KW_REAL
						|	KW_BOOLEAN
						|	ArrayType
						|	PointerType
						|	ProcedureType

ArrayType				:  KW_ARRAY lengthList KW_OF type

lenghtList : 	lenght
						|	lenght ',' lenghtList

lenght					:	ConstExpression

PointerType				: 	KW_POINTER_TO type

ProcedureType 			:	KW_PROCEDURE
						|	KW_PROCEDURE FormalParameters

VariableDeclaration		:	IdentifiersList ':' type

ConstantDeclaration		: 	identifier '=' ConstExpression

ConstExpression			: 	expression

designator				:	identifier
						|	identifier designatorHelper

designatorHelper		: 	'.' designator
						|	'[' ExpList ']'
						|	'[' ExpList ']' designatorHelper

ExpList					: 	expression
						|	expression ',' ExpList

expression 				: 	SimpleExpression
						| 	SimpleExpression relation SimpleExpression

relation				: 	'='
						| 	'#'
						| 	'<'
						| 	'<='
						| 	'>'
						| 	'>='

SimpleExpression		:	term
						|	term AddOperatorList
						|	'+' term
						|	'-' term
						|	'+' term AddOperatorList
						|	'-' term AddOperatorList

AddOperator				:	'+'
						|	'-'
						|	KW_OR

AddOperatorList			:	AddOperator term
						|	AddOperator term AddOperatorList

term 					:	factor
						|	factor MulOperatorList

MulOperator 			:	'*'
						|	'/'
						|	KW_DIV
						| 	KW_MOD
						|	'&'

MulOperatorList			:	MulOperator factor
						|	MulOperator factor MulOperatorList

factor	 				:	integerNum
						|	realNum
						|	'"' validChar '"'
						|	'"' validString '"'
						|	designator
						|	designator ActualParameters
						|	"(" expression ")"
						|	"~" factor

ActualParameters		: 	"(" ")"
						|	"(" ExpList ")"

statement 				:	assignment
						|	ProcedureCall
						|	IfStatement
						|	CaseStatement
						| 	WhileStatement
						|	RepeatStatement
						|	LoopStatement
						|	KW_EXIT
						|	KW_RETURN
						|	KW_RETURN expression

assignment 				:	designator ':=' expression

ProcedureCall 			:	designator
						|	designator ActualParameters

StatementSequence 		:	statement
						|	statement ';' StatementSequence

IfStatement 			:	KW_IF expression KW_THEN StatementSequence KW_END
						|	KW_IF expression KW_THEN StatementSequence KW_ELSE StatementSequence KW_END
						|	KW_IF expression KW_THEN StatementSequence ElseIfList KW_END
						|	KW_IF expression KW_THEN StatementSequence ElseIfList KW_ELSE StatementSequence KW_END

ElseIfList 				:	KW_ELSIF expression KW_THEN StatementSequence
						|	KW_ELSIF expression KW_THEN StatementSequence ElseIfList

CaseStatement 			: 	KW_CASE expression KW_OF Case KW_END
						|	KW_CASE expression KW_OF Case KW_ELSE StatementSequence KW_END
						|	KW_CASE expression KW_OF CaseList KW_END
						|	KW_CASE expression KW_OF CaseList KW_ELSE StatementSequence KW_END

Case 					:	CaseLabelList ':' StatementSequence

CaseLabelList 			:	CaseLabels
						|	CaseLabels ',' CaseLabelList

CaseLabels 				:	ConstExpression
						|	ConstExpression '..' ConstExpression

WhileStatement 			:	KW_WHILE expression KW_DO StatementSequence KW_END

RepeatStatement			:	KW_REPEAT StatementSequence KW_UNTIL expression

LoopStatement 			:	KW_LOOP StatementSequence KW_END

ProcedureDeclaration	:	ProcedureHeading ';' ProcedureBody identifier

ProcedureHeading		:	KW_PROCEDURE identifier 								{
																						do
																							let tempStack = createProcedure $2 proceduresStack
																							let proceduresStack = tempStack
																					}
						| 	KW_PROCEDURE identifier FormalParameters

ProcedureBody			: 	DeclarationSequence KW_END
						|	DeclarationSequence KW_BEGIN StatementSequence KW_END

DeclarationSequence		:	KW_CONST ConstDeclaration ';'
						|	KW_VAR VariableDeclaration ';'
						|	ProcedureDeclaration

FormalParameters 		:	'(' ')'
						|	'(' FPSectionList ')'
						|	'(' FPSectionList ')' ':' type

FPSection 				:	IdentifiersList ':' FormalType
						|	KW_VAR IdentifiersList ':' FormalType

FPSectionList 			: 	FPSection
						|	FPSection ';' FPSectionList

FormalType 				: 	baseTypes
						|	KW_ARRAY KW_OF baseTypes

{
parseError :: [Token] -> a
parseError _ = error "Parse errore"

main = do
	inStr <- getContents
	let result = newl (alexScanTokens inStr)
}
