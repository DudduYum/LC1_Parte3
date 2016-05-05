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
  '='                   { KW_TokenEqual }
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
  '"'                   { KW_TokenDoubleQuotes }
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


ProcedureBody     : KW_END                                          { [] }
                  | DeclarationSequenceList KW_END                  { $1 }
--            | DeclarationSequence KW_BEGIN StatementSequence KW_END   { }

DeclarationSequence   : KW_VAR VariableDeclarationList       { $2 }
                      | KW_CONST ConstDeclarationList        { $2 }
                      | ProcedureDeclarationList             { $1 }

DeclarationSequenceList : DeclarationSequence                         { $1 }
                        | DeclarationSequence DeclarationSequenceList { $1++$2 }

ConstDeclarationList  : ConstDeclaration ';'                        { [$1] }
                      | ConstDeclaration ';' ConstDeclarationList   { $1:$3 }

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
                                                        parseError [KW_TokenAssignment]
                                                      else
                                                        Array lenList $4
                                                  }
--						|	PointerType
--						|	ProcedureType

lenghtList 				: 	lenght                  {
                                                checkIndex $1
                                                -- do
                                                --   let val = $1
                                                --   if attributeIsOfType val (Simple Integer) then
                                                --     [(integerValue val)]
                                                --   else
                                                --     parseError [KW_TokenStar]
                                              }
						      |	  lenght ',' lenghtList   { (checkIndex $1)++$3 }

lenght					:	ConstExpression     { $1 }

--PointerType				: 	KW_POINTER_TO type

--ProcedureType 			:	KW_PROCEDURE
--						|	KW_PROCEDURE FormalParameters

ConstDeclaration		: 	identifier '=' ConstExpression  {
                                                          do
                                                            let exprResult = $3
                                                            defaultDeclaration {  declarationType = DT_Constant,
                                                                                  attributeDeclared = Just defaultAttribute { attributeName = $1,
                                                                                                                              attributeType = (attributeType exprResult),
                                                                                                                              stringValue = (stringValue exprResult),
                                                                                                                              floatValue = (floatValue exprResult),
                                                                                                                              integerValue = (integerValue exprResult),
                                                                                                                              charValue = (charValue exprResult),
                                                                                                                              booleanValue = (booleanValue exprResult),
                                                                                                                              stringArrayValue = (stringArrayValue exprResult),
                                                                                                                              floatArrayValue = (floatArrayValue exprResult),
                                                                                                                              integerArrayValue = (integerArrayValue exprResult),
                                                                                                                              charArrayValue = (charArrayValue exprResult),
                                                                                                                              booleanArrayValue = (booleanArrayValue exprResult),
                                                                                                                              isConstant = True } }
                                                        }

ConstExpression			  : 	expression      { $1 }

--designator				:	identifier
--						|	identifier designatorHelper

--designatorHelper		: 	'.' designator
--						|	'[' ExpList ']'
--						|	'[' ExpList ']' designatorHelper

--ExpList					: 	expression
--						|	expression ',' ExpList

expression 		: 	SimpleExpression                              { $1 }
						  | 	SimpleExpression relation SimpleExpression    {
                                                                  do
                                                                    let expRes1 = $1
                                                                    let expRes2 = $3
                                                                    let rel = $2

                                                                    if not (attributesSameType expRes1 expRes2) then
                                                                      parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenEqual then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) == (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) == (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((charValue expRes1) == (charValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = (stringsEqual (stringValue expRes1) (stringValue expRes2)) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenDiesis then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((floatValue expRes1) == (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((integerValue expRes1) == (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((charValue expRes1) == (charValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = not (stringsEqual (stringValue expRes1) (stringValue expRes2)) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenMinor then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) < (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) < (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) < (fromEnum (charValue expRes2))) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) < (length (stringValue expRes2))) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenMinorEqual then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) <= (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) <= (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) <= (fromEnum (charValue expRes2))) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) <= (length (stringValue expRes2))) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenMajor then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) > (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) > (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) > (fromEnum (charValue expRes2))) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) > (length (stringValue expRes2))) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else if rel == KW_TokenMajorEqual then
                                                                      if attributeIsOfType expRes1 (Simple Float) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) >= (floatValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Integer) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) >= (integerValue expRes2)) }
                                                                      else if attributeIsOfType expRes1 (Simple Char) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) >= (fromEnum (charValue expRes2))) }
                                                                      else if attributeIsOfType expRes1 (Simple String) then
                                                                        defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) >= (length (stringValue expRes2))) }
                                                                      else
                                                                        parseError [KW_TokenTilde]
                                                                    else
                                                                      parseError [KW_TokenTilde]
                                                                }

relation		: 	'='            { $1 }
						| 	'#'            { $1 }
						| 	'<'            { $1 }
						| 	KW_MinorEqual  { $1 }
						| 	'>'            { $1 }
						| 	KW_MajorEqual  { $1 }

SimpleExpression		:	term                        { $1 }
                    | '+' SimpleExpression        { $2 }
                    | '-' SimpleExpression        {
                                                    do
                                                      let val = $2
                                                      if attributeIsOfType val (Simple Integer) then
                                                        defaultAttribute { attributeType = Simple Integer, integerValue = -(integerValue val) }
                                                      else
                                                        parseError [KW_TokenChar]
                                                  }
						        |	term '+' SimpleExpression   {
                                                    do
                                                      let t1 = $1
                                                      let t2 = $3

                                                      if attributesSameType t1 t2 then
                                                        -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                        if attributeIsOfType t1 (Simple Float) then
                                                          defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) + (floatValue t2) }
                                                        else if attributeIsOfType t1 (Simple Integer) then
                                                          defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) + (integerValue t2) }
                                                        else if attributeIsOfType t1 (Simple String) then
                                                          defaultAttribute { attributeType = Simple String, stringValue = (stringValue t1) ++ (stringValue t2) }
                                                        else
                                                          parseError [KW_TokenColon]
                                                      else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                        defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) + (floatValue t2) }
                                                      else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                        defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) + (fromIntegral (integerValue t2)) }
                                                      else
                                                        parseError [KW_TokenPoint]
                                                  }
                    | term '-' SimpleExpression   {
                                                    do
                                                      let t1 = $1
                                                      let t2 = $3

                                                      if attributesSameType t1 t2 then
                                                        -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                        if attributeIsOfType t1 (Simple Float) then
                                                          defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) - (floatValue t2) }
                                                        else if attributeIsOfType t1 (Simple Integer) then
                                                          defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) - (integerValue t2) }
                                                        else
                                                          parseError [KW_TokenColon]
                                                      else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                        defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) - (floatValue t2) }
                                                      else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                        defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) - (fromIntegral (integerValue t2)) }
                                                      else
                                                        parseError [KW_TokenPoint]
                                                  }
                    | term KW_OR SimpleExpression {
                                                    do
                                                      let t1 = $1
                                                      let t2 = $3

                                                      if (attributeType t1 == Simple Boolean) && (attributeType t2 == Simple Boolean) then
                                                          defaultAttribute { attributeType = Simple Boolean, booleanValue = (booleanValue t1) || (booleanValue t2) }
                                                      else
                                                        parseError [KW_TokenPoint]
                                                  }

term 					:	factor                { $1 }
						  |	factor '*' term       {
                                        do
                                          let t1 = $1
                                          let t2 = $3

                                          if attributesSameType t1 t2 then
                                            -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                            if attributeIsOfType t1 (Simple Float) then
                                              defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) * (floatValue t2) }
                                            else if attributeIsOfType t1 (Simple Integer) then
                                              defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) * (integerValue t2) }
                                            else
                                              parseError [KW_TokenColon]
                                          else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                            defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) * (floatValue t2) }
                                          else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                            defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) * (fromIntegral (integerValue t2)) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }
              | factor '/' term       {
                                        do
                                          let t1 = $1
                                          let t2 = $3

                                          if attributesSameType t1 t2 then
                                            -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                            if attributeIsOfType t1 (Simple Float) then
                                              defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) / (floatValue t2) }
                                            else if attributeIsOfType t1 (Simple Integer) then
                                              defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) / (fromIntegral (integerValue t2)) }
                                            else
                                              parseError [KW_TokenColon]
                                          else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                            defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) / (floatValue t2) }
                                          else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                            defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) / (fromIntegral (integerValue t2)) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }
              | factor KW_DIV term    {
                                        do
                                          let t1 = $1
                                          let t2 = $3

                                          if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Integer) then
                                            defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) `quot` (integerValue t2) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }
              | factor KW_MOD term    {
                                        do
                                          let t1 = $1
                                          let t2 = $3

                                          if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Integer) then
                                            defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) `mod` (integerValue t2) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }
              | factor '&' term       {
                                        do
                                          let t1 = $1
                                          let t2 = $3

                                          if (attributeType t1 == Simple Boolean) && (attributeType t2 == Simple Boolean) then
                                            defaultAttribute { attributeType = Simple Boolean, booleanValue = (booleanValue t1) && (booleanValue t2) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }

factor	 				:	integerNum          { defaultAttribute { attributeType = Simple Integer, integerValue = $1 } }
						    |	realNum             { defaultAttribute { attributeType = Simple Float, floatValue = $1 } }
                | KW_TRUE             { defaultAttribute { attributeType = Simple Boolean, booleanValue = True } }
                | KW_FALSE            { defaultAttribute { attributeType = Simple Boolean, booleanValue = False } }
						    |	'"' validChar '"'   { defaultAttribute { attributeType = Simple Char, charValue = $2 } }
						    |	'"' validString '"' { defaultAttribute { attributeType = Simple String, stringValue = $2 } }
						    |	'(' expression ')'  { $2 }
    						|	'~' factor          {
                                        do
                                          let val = $2

                                          if attributeType val == Simple Boolean then
                                            defaultAttribute { attributeType = Simple Boolean, booleanValue = not (booleanValue val) }
                                          else
                                            parseError [KW_TokenPoint]
                                      }

--                | designator
--                | designator ActualParameters

--ActualParameters		: 	'(' ')'
--						|	'(' ExpList ')'

--statement 	:	assignment
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

checkIndex v = do
  let val = v
  if attributeIsOfType val (Simple Integer) then
    [(integerValue val)]
  else
    parseError [KW_TokenStar]

main = do
  inStr <- getContents
  --print (alexScanTokens inStr)
  let result = oLikeParse (alexScanTokens inStr)
  putStrLn ("result: " ++ show(result))
  putStrLn("DONE")
}
