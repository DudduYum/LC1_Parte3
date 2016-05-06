{
module Main where
import OberonLexer
import OberonTools
}

%name oLikeParse
%error { parseError }
%lexer { lexwrap } { EOF }
%monad { Alex }
%tokentype { Token }
%token
  KW_INTEGER            { KW_TokenInteger _ }
  KW_REAL               { KW_TokenReal _ }
  KW_BOOLEAN            { KW_TokenBoolean _ }
  KW_CHAR               { KW_TokenChar _ }
  KW_ARRAY              { KW_TokenArray _ }
  KW_OF                 { KW_TokenOf _ }
  KW_POINTER_TO         { KW_TokenPointerTo _ }
  KW_PROCEDURE          { KW_TokenProcedure _ }
  KW_BEGIN              { KW_TokenBegin _ }
  KW_END                { KW_TokenEnd _ }
  KW_VAR                { KW_TokenVar _ }
  KW_CONST              { KW_TokenConst _ }
  KW_TRUE               { KW_TokenTrue _ }
  KW_FALSE              { KW_TokenFalse _ }
  KW_IF                 { KW_TokenIf _ }
  KW_ELSIF              { KW_TokenElsif _ }
  KW_ELSE               { KW_TokenElse _ }
  KW_THEN               { KW_TokenThen _ }
  KW_CASE               { KW_TokenCase _ }
  KW_WHILE              { KW_TokenWhile _ }
  KW_DO                 { KW_TokenDo _ }
  KW_REPEAT             { KW_TokenRepeat _ }
  KW_UNTIL             	{ KW_TokenUntil _ }
  KW_LOOP               { KW_TokenLoop _ }
  KW_EXIT               { KW_TokenExit _ }
  KW_RETURN             { KW_TokenReturn _ }
  KW_BREAK              { KW_TokenBreak _ }
  KW_CONTINUE           { KW_TokenContinue _ }
  KW_OR                 { KW_TokenOr _ }
  '&'                   { KW_TokenCommercialE _ }
  '~'                   { KW_TokenTilde _ }
  '+'                   { KW_TokenPlus _ }
  '-'                   { KW_TokenMinus _ }
  '*'                   { KW_TokenStar _ }
  '/'                   { KW_TokenForwardSlash _ }
  KW_DIV                { KW_TokenDiv _ }
  KW_MOD                { KW_TokenMod _ }
  '='                   { KW_TokenEqual _ }
  '#'                   { KW_TokenDiesis _ }
  '<'                   { KW_TokenMinor _ }
  KW_MinorEqual         { KW_TokenMinorEqual _ }
  '>'                   { KW_TokenMajor _ }
  KW_MajorEqual         { KW_TokenMajorEqual _ }
  KW_Assignment         { KW_TokenAssignment _ }
  '.'					          { KW_TokenPoint _ }
  ','         					{ KW_TokenComa _ }
  ':'					          { KW_TokenColon _ }
  ';'         					{ KW_TokenSemiColon _ }
  '('         					{ KW_TokenOpenBracket _ }
  ')'         					{ KW_TokenClosedBracket _ }
  '['         					{ KW_TokenOpenSquareBracket _ }
  ']'         					{ KW_TokenClosedSquareBracket _ }
  '|'                   { KW_TokenPipe _ }
  '"'                   { KW_TokenDoubleQuotes _ }
  identifier 			      { TokenVariableIdentifier name _ }
  integerNum            { TokenIntegerNumber intVal _ }
  realNum 				      { TokenRealNumber fltVal _ }
  validChar             { TokenValidChar chrVal _ }
  validString        	  { TokenValidString strVal _ }

%%

ProcedureDeclarationList  :   ProcedureDeclaration                              { [$1] }
                          |   ProcedureDeclaration ';' ProcedureDeclarationList { $1:$3 }

ProcedureDeclaration  : ProcedureHeading ';' ProcedureBody identifier {
                                                                        do
                                                                          let newProc = $1
                                                                          if (procedureName newProc) == (name $4) then
                                                                            defaultDeclaration { declarationType = DT_Procedure, procedureDeclared = Just (addBodyToProcedure newProc $3) }
                                                                          else
                                                                            fatalError ("END procedure name " ++ (show (name $4)) ++ " not corresponding to the initial declaration " ++ (show (procedureName newProc))) (getRow (position $4)) (getCol (position $4))
                                                                      }

IdentifiersList 		: 	identifier							        { [(name $1)] }
						        |	  identifier ',' IdentifiersList  { (name $1):$3 }

VariableDeclaration :   IdentifiersList ':' type        { createVariablesDefinitionsOfType $1 $3 }

ProcedureHeading    :   KW_PROCEDURE identifier                                 { defaultProcedure { procedureName = (name $2) } }
                    |   KW_PROCEDURE identifier FormalParameters                { addParametersToProcedure (defaultProcedure { procedureName = (name $2) }) $3 }
                    |   KW_PROCEDURE identifier FormalParameters ':' FormalType { addParametersToProcedure (defaultProcedure { procedureName = (name $2), returnType = Just $5 }) $3 }

FormalParameters    :   '(' ')'                         { [] }
                    |   '(' FPSectionList ')'           { $2 }

FPSection           :   IdentifiersList ':' FormalType          { createProcedureParametersByReferenceDefinitionsOfType $1 $3 }
                    |   KW_VAR IdentifiersList ':' FormalType   { createProcedureParametersByValueDefinitionsOfType $2 $4 }

FPSectionList       :   FPSection                       { $1 }
                    |   FPSection ';' FPSectionList     { $1++$3 }

FormalType          : KW_INTEGER                  { Simple Integer }
                    | KW_REAL                     { Simple Float }
                    | KW_BOOLEAN                  { Simple Boolean }
                    | KW_CHAR                     { Simple Char }
                    | KW_ARRAY KW_OF FormalType   { UnsizedArray $3 }
--                    | KW_POINTER_TO

ProcedureBody     : KW_END                                                    { [] }
                  | DeclarationSequenceList KW_END                            { $1 }
                  | DeclarationSequenceList KW_BEGIN StatementSequence KW_END { $1++$3 }

DeclarationSequence   : KW_VAR VariableDeclarationList       { $2 }
                      | KW_CONST ConstDeclarationList        { $2 }
                      | ProcedureDeclarationList             { $1 }

DeclarationSequenceList : DeclarationSequence                         { $1 }
                        | DeclarationSequence DeclarationSequenceList { $1++$2 }

StatementSequence   : statement ';'                     { [$1] }
                    | statement ';' StatementSequence   { $1:$3 }

LoopStatementSequence : statement ';'                         { [$1] }
                      | KW_BREAK ';'                          { [(defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just OP_Break })] }
                      | KW_CONTINUE ';'                       { [(defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just OP_Continue })] }
                      | statement ';' LoopStatementSequence   { [$1]++$3 }
                      | KW_BREAK ';' LoopStatementSequence    { [(defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just OP_Break })]++$3 }
                      | KW_CONTINUE ';' LoopStatementSequence { [(defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just OP_Continue })]++$3 }


statement   :   designator KW_Assignment expression     { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Assignment $1 $3) } }
            |   ProcedureCall                           { $1 }
            |   IfStatement                             { $1 }
            |   CaseStatement                           { $1 }
            |   KW_WHILE expression KW_DO LoopStatementSequence KW_END    { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_While $2 (declarationListToOperationList $4) ) } }
            |   KW_REPEAT LoopStatementSequence KW_UNTIL expression       { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Repeat (declarationListToOperationList $2) $4 ) } }
            |   KW_LOOP LoopStatementSequence KW_END                      { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Loop (declarationListToOperationList $2) ) } }
            |   KW_EXIT                                   { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just OP_Exit } }
            |   KW_RETURN                                 { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Return Nothing) } }
            |   KW_RETURN expression                      { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Return (Just $2)) } }

ProcedureCall : designator                      { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_ProcedureCall $1 []) } }
              | designator ActualParameters     { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_ProcedureCall $1 $2) } }

ActualParameters  : '(' ')'           { [] }
                  | '(' ExpList ')'   { $2 }

IfStatement : KW_IF expression KW_THEN StatementSequence KW_END                                       { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_If ($2, (declarationListToOperationList $4)) ) } }
            | KW_IF expression KW_THEN StatementSequence KW_ELSE StatementSequence KW_END             { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_If_Else ($2, (declarationListToOperationList $4), (declarationListToOperationList $6)) ) } }
            | KW_IF expression KW_THEN StatementSequence ElseIfList KW_END                            { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_If_Elsif ([($2, (declarationListToOperationList $4))]++$5)) } }
            | KW_IF expression KW_THEN StatementSequence ElseIfList KW_ELSE StatementSequence KW_END  { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_If_Elsif_Else ([($2, (declarationListToOperationList $4))]++$5, (declarationListToOperationList $7))) } }

ElseIfList  : KW_ELSIF expression KW_THEN StatementSequence               { [($2, (declarationListToOperationList $4))] }
            | KW_ELSIF expression KW_THEN StatementSequence ElseIfList    { [($2, (declarationListToOperationList $4))]++$5}

CaseStatement   :   KW_CASE expression KW_OF CaseList KW_END                            { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Case ($2, $4)) } }
                |   KW_CASE expression KW_OF CaseList KW_ELSE StatementSequence KW_END  { defaultDeclaration { declarationType = DT_Operation, operationDeclared = Just (OP_Case_Else ($2, $4, (declarationListToOperationList $6))) } }

Case  :   ConstExpression ':' StatementSequence   { ($1, (declarationListToOperationList $3)) }

CaseList  : Case                { [$1] }
          | Case '|' CaseList   { $1:$3 }

ConstDeclarationList  : ConstDeclaration ';'                        { [$1] }
                      | ConstDeclaration ';' ConstDeclarationList   { $1:$3 }

VariableDeclarationList : VariableDeclaration ';'                         { $1 }
                        | VariableDeclaration ';' VariableDeclarationList { $1++$3 }

type 				: 	KW_INTEGER                        { Simple Integer }
						|	  KW_REAL                           { Simple Float }
						|	  KW_BOOLEAN                        { Simple Boolean }
            |   KW_CHAR                           { Simple Char }
						|   KW_ARRAY lenghtList KW_OF type    {
                                                    do
                                                      let lenList = $2
                                                      if listElementIsLessOrEqualZero lenList then
                                                        fatalError ("Invalid array size. One or more array sizes are less than or equal to 0") (getRow (position $1)) (getCol (position $1))
                                                      else
                                                        createMultidimensionalArrayOfType lenList $4
                                                  }
--						|	PointerType

lenghtList 				: 	lenght                  { checkIndex $1 }
						      |	  lenght ',' lenghtList   { (checkIndex $1)++$3 }

lenght	:	ConstExpression     { $1 }

--PointerType				: 	KW_POINTER_TO type

ConstDeclaration		: 	identifier '=' ConstExpression  {
                                                          do
                                                            let exprResult = $3
                                                            defaultDeclaration {  declarationType = DT_Constant,
                                                                                  attributeDeclared = Just defaultAttribute { attributeName = (name $1),
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

designator    :	  identifier                      { defaultAttribute { attributeType = Simple Name, nameValue = (name $1) } }
						  |	  identifier designatorHelper     { defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_arr_ext (defaultAttribute { attributeType = Simple Name, nameValue = (name $1) }) $2) } }

designatorHelper	:   '[' expression ']'                    { [$2] }
						      |	  '[' expression ']' designatorHelper   { $2:$4 }

ExpList		:  expression              { [$1] }
					|  expression ',' ExpList  { $1:$3 }

expression 		: 	SimpleExpression                                { $1 }
						  | 	SimpleExpression '=' SimpleExpression           {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_eq expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) == (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) == (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((charValue expRes1) == (charValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = (stringsEqual (stringValue expRes1) (stringValue expRes2)) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }
              |   SimpleExpression '#' SimpleExpression           {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_neq expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((floatValue expRes1) == (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((integerValue expRes1) == (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = not ((charValue expRes1) == (charValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = not (stringsEqual (stringValue expRes1) (stringValue expRes2)) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }
              |   SimpleExpression '<' SimpleExpression           {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_min expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) < (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) < (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) < (fromEnum (charValue expRes2))) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) < (length (stringValue expRes2))) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }
              |   SimpleExpression KW_MinorEqual SimpleExpression {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_mineq expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) <= (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) <= (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) <= (fromEnum (charValue expRes2))) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) <= (length (stringValue expRes2))) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }
              |   SimpleExpression '>' SimpleExpression           {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_maj expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) > (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) > (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) > (fromEnum (charValue expRes2))) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) > (length (stringValue expRes2))) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }
              |   SimpleExpression KW_MajorEqual SimpleExpression {
                                                                    do
                                                                      let tmp1 = getOperationResult (Just $1)
                                                                      let tmp2 = getOperationResult (Just $3)
                                                                      
                                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                                      else do
                                                                              let expRes1 = getMaybeValue tmp1
                                                                              let expRes2 = getMaybeValue tmp2

                                                                              if (attributeType expRes1 == Simple Unknown) || (attributeType expRes2 == Simple Unknown) || (attributeType expRes1 == Simple Name) || (attributeType expRes2 == Simple Name) then
                                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_majeq expRes1 expRes2) }
                                                                              else if not (attributesSameType expRes1 expRes2) then
                                                                                fatalError ("Invalid operation. Cannot compare two different types") (getRow (position $2)) (getCol (position $2))
                                                                              else
                                                                                if attributeIsOfType expRes1 (Simple Float) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((floatValue expRes1) >= (floatValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Integer) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((integerValue expRes1) >= (integerValue expRes2)) }
                                                                                else if attributeIsOfType expRes1 (Simple Char) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((fromEnum (charValue expRes1)) >= (fromEnum (charValue expRes2))) }
                                                                                else if attributeIsOfType expRes1 (Simple String) then
                                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = ((length (stringValue expRes1)) >= (length (stringValue expRes2))) }
                                                                                else
                                                                                  fatalError ("Invalid operation. Equality comparison is possible only between two FLOAT, INTEGER, CHAR or STRING") (getRow (position $2)) (getCol (position $2))
                                                                  }

SimpleExpression		:	term                        { $1 }
                    | '+' SimpleExpression        {
                                                    do
                                                      let tmp1 = getOperationResult (Just $2)

                                                      if tmp1 == Nothing then
                                                        fatalError ("Invalid operation.") (getRow (position $1)) (getCol (position $1))
                                                      else do
                                                              let val = getMaybeValue tmp1

                                                              if (attributeType val == Simple Unknown) || (attributeType val == Simple Name) then
                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_iden_add val) }
                                                              else if (attributeIsOfType val (Simple Integer)) || (attributeIsOfType val (Simple Float)) then
                                                                val
                                                              else
                                                                fatalError ("Invalid operation. Identity operation is possible only with INTEGER or FLOAT") (getRow (position $1)) (getCol (position $1))
                                                  }
                    | '-' SimpleExpression        {
                                                    do
                                                      let tmp1 = getOperationResult (Just $2)

                                                      if tmp1 == Nothing then
                                                        fatalError ("Invalid operation.") (getRow (position $1)) (getCol (position $1))
                                                      else do
                                                              let val = getMaybeValue tmp1
                                                      
                                                              if (attributeType val == Simple Unknown) || (attributeType val == Simple Name) then
                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_iden_sub val) }
                                                              else if attributeIsOfType val (Simple Integer) then
                                                                defaultAttribute { attributeType = Simple Integer, integerValue = -(integerValue val) }
                                                              else if attributeIsOfType val (Simple Float) then
                                                                defaultAttribute { attributeType = Simple Float, floatValue = -(floatValue val) }
                                                              else
                                                                fatalError ("Invalid operation. Identity operation is possible only with INTEGER or FLOAT") (getRow (position $1)) (getCol (position $1))
                                                  }
						        |	term '+' SimpleExpression   {
                                                    do
                                                      let tmp1 = getOperationResult (Just $1)
                                                      let tmp2 = getOperationResult (Just $3)

                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                      else do
                                                              let t1 = getMaybeValue tmp1
                                                              let t2 = getMaybeValue tmp2

                                                              if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_add t1 t2) }
                                                              else if attributesSameType t1 t2 then
                                                                -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                                if attributeIsOfType t1 (Simple Float) then
                                                                  defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) + (floatValue t2) }
                                                                else if attributeIsOfType t1 (Simple Integer) then
                                                                  defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) + (integerValue t2) }
                                                                else if attributeIsOfType t1 (Simple String) then
                                                                  defaultAttribute { attributeType = Simple String, stringValue = (stringValue t1) ++ (stringValue t2) }
                                                                else
                                                                  fatalError ("Invalid operation. Sum operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                              else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                                defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) + (floatValue t2) }
                                                              else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                                defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) + (fromIntegral (integerValue t2)) }
                                                              else
                                                                fatalError ("Invalid operation. Sum operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                  }
                    | term '-' SimpleExpression   {
                                                    do
                                                      let tmp1 = getOperationResult (Just $1)
                                                      let tmp2 = getOperationResult (Just $3)

                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                      else do
                                                              let t1 = getMaybeValue tmp1
                                                              let t2 = getMaybeValue tmp2

                                                              if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_sub t1 t2) }
                                                              else if attributesSameType t1 t2 then
                                                                -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                                if attributeIsOfType t1 (Simple Float) then
                                                                  defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) - (floatValue t2) }
                                                                else if attributeIsOfType t1 (Simple Integer) then
                                                                  defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) - (integerValue t2) }
                                                                else
                                                                  fatalError ("Invalid operation. Subtract operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                              else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                                defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) - (floatValue t2) }
                                                              else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                                defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) - (fromIntegral (integerValue t2)) }
                                                              else
                                                                fatalError ("Invalid operation. Subtract operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                  }
                    | term KW_OR SimpleExpression {
                                                    do
                                                      let tmp1 = getOperationResult (Just $1)
                                                      let tmp2 = getOperationResult (Just $3)

                                                      if tmp1 == Nothing || tmp2 == Nothing then
                                                        fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                                      else do
                                                              let t1 = getMaybeValue tmp1
                                                              let t2 = getMaybeValue tmp2

                                                              if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                                defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_or t1 t2) }
                                                              else if (attributeType t1 == Simple Boolean) && (attributeType t2 == Simple Boolean) then
                                                                  defaultAttribute { attributeType = Simple Boolean, booleanValue = (booleanValue t1) || (booleanValue t2) }
                                                              else
                                                                fatalError ("Invalid operation. Logic OR operation is valid only between two BOOLEAN") (getRow (position $2)) (getCol (position $2))
                                                  }

term 					:	factor                { $1 }
						  |	factor '*' term       {
                                        do
                                          let tmp1 = getOperationResult (Just $1)
                                          let tmp2 = getOperationResult (Just $3)

                                          if tmp1 == Nothing || tmp2 == Nothing then
                                            fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                          else do
                                                  let t1 = getMaybeValue tmp1
                                                  let t2 = getMaybeValue tmp2

                                                  if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                    defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_mul t1 t2) }
                                                  else if attributesSameType t1 t2 then
                                                    -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                    if attributeIsOfType t1 (Simple Float) then
                                                      defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) * (floatValue t2) }
                                                    else if attributeIsOfType t1 (Simple Integer) then
                                                      defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) * (integerValue t2) }
                                                    else
                                                      fatalError ("Invalid operation. Multiplication operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                  else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                    defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) * (floatValue t2) }
                                                  else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                    defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) * (fromIntegral (integerValue t2)) }
                                                  else
                                                    fatalError ("Invalid operation. Multiplication operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                      }
              | factor '/' term       {
                                        do
                                          let tmp1 = getOperationResult (Just $1)
                                          let tmp2 = getOperationResult (Just $3)

                                          if tmp1 == Nothing || tmp2 == Nothing then
                                            fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                          else do
                                                  let t1 = getMaybeValue tmp1
                                                  let t2 = getMaybeValue tmp2

                                                  if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                    defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_div t1 t2) }
                                                  else if attributesSameType t1 t2 then
                                                    -- basta controllare uno solo dei due operandi perche' so che sono dello stesso tipo
                                                    if attributeIsOfType t1 (Simple Float) then
                                                      defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) / (floatValue t2) }
                                                    else if attributeIsOfType t1 (Simple Integer) then
                                                      defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) / (fromIntegral (integerValue t2)) }
                                                    else
                                                      fatalError ("Invalid operation. Division operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                                  else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Float) then
                                                    defaultAttribute { attributeType = Simple Float, floatValue = (fromIntegral (integerValue t1)) / (floatValue t2) }
                                                  else if (attributeType t1 == Simple Float) && (attributeType t2 == Simple Integer) then
                                                    defaultAttribute { attributeType = Simple Float, floatValue = (floatValue t1) / (fromIntegral (integerValue t2)) }
                                                  else
                                                    fatalError ("Invalid operation. Division operation is valid only between INTEGER-INTEGER, FLOAT-FLOAT, INTEGER-FLOAT or FLOAT-INTEGER") (getRow (position $2)) (getCol (position $2))
                                      }
              | factor KW_DIV term    {
                                        do
                                          let tmp1 = getOperationResult (Just $1)
                                          let tmp2 = getOperationResult (Just $3)

                                          if tmp1 == Nothing || tmp2 == Nothing then
                                            fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                          else do
                                                  let t1 = getMaybeValue tmp1
                                                  let t2 = getMaybeValue tmp2

                                                  if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                    defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_quot t1 t2) }
                                                  else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Integer) then
                                                    defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) `quot` (integerValue t2) }
                                                  else
                                                    fatalError ("Invalid operation. Quotient operation is valid only between two INTEGER") (getRow (position $2)) (getCol (position $2))
                                      }
              | factor KW_MOD term    {
                                        do
                                          let tmp1 = getOperationResult (Just $1)
                                          let tmp2 = getOperationResult (Just $3)

                                          if tmp1 == Nothing || tmp2 == Nothing then
                                            fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                          else do
                                                  let t1 = getMaybeValue tmp1
                                                  let t2 = getMaybeValue tmp2

                                                  if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                    defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_mod t1 t2) }
                                                  else if (attributeType t1 == Simple Integer) && (attributeType t2 == Simple Integer) then
                                                    defaultAttribute { attributeType = Simple Integer, integerValue = (integerValue t1) `mod` (integerValue t2) }
                                                  else
                                                    fatalError ("Invalid operation. Modulus operation is valid only between two INTEGER") (getRow (position $2)) (getCol (position $2))
                                      }
              | factor '&' term       {
                                        do
                                          let tmp1 = getOperationResult (Just $1)
                                          let tmp2 = getOperationResult (Just $3)

                                          if tmp1 == Nothing || tmp2 == Nothing then
                                            fatalError ("Invalid operation.") (getRow (position $2)) (getCol (position $2))
                                          else do
                                                  let t1 = getMaybeValue tmp1
                                                  let t2 = getMaybeValue tmp2

                                                  if (attributeType t1 == Simple Unknown) || (attributeType t2 == Simple Unknown) || (attributeType t1 == Simple Name) || (attributeType t2 == Simple Name) then
                                                    defaultAttribute { attributeType = Simple Unknown, basicOperation = Just (OP_and t1 t2) }
                                                  else if (attributeType t1 == Simple Boolean) && (attributeType t2 == Simple Boolean) then
                                                    defaultAttribute { attributeType = Simple Boolean, booleanValue = (booleanValue t1) && (booleanValue t2) }
                                                  else
                                                    fatalError ("Invalid operation. Logical AND operation is valid only between two BOOLEAN") (getRow (position $2)) (getCol (position $2))
                                      }

factor	 				:	integerNum                  { defaultAttribute { attributeType = Simple Integer, integerValue = (intVal $1) } }
						    |	realNum                     { defaultAttribute { attributeType = Simple Float, floatValue = (fltVal $1) } }
                | KW_TRUE                     { defaultAttribute { attributeType = Simple Boolean, booleanValue = True } }
                | KW_FALSE                    { defaultAttribute { attributeType = Simple Boolean, booleanValue = False } }
						    |	validChar                   { defaultAttribute { attributeType = Simple Char, charValue = (chrVal $1) } }
						    |	validString                 { defaultAttribute { attributeType = Simple String, stringValue = (strVal $1) } }
						    |	'(' expression ')'          { $2 }
    						|	'~' factor                  {
                                                do
                                                  let val = $2

                                                  if attributeType val == Simple Boolean then
                                                    defaultAttribute { attributeType = Simple Boolean, booleanValue = not (booleanValue val) }
                                                  else
                                                    fatalError ("Invalid operation. Logical NOT operation is valid only with a BOOLEAN") (getRow (position $1)) (getCol (position $1))
                                              }
                | designator                  { $1 }
                | designator ActualParameters { $1 { isProcedureCall = True, procCallParams = $2 } }

{

checkIndex v = do
  let val = v
  if attributeIsOfType val (Simple Integer) then
    [(integerValue val)]
  else
    fatalError_2 ("Invalid array size. Only integer numbers are valid array sizes")

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

getCol :: AlexPosn -> Int
getCol (AlexPn _ _ c) = c

getRow :: AlexPosn -> Int
getRow (AlexPn _ b _) = b

parseError :: Token -> Alex a
parseError tk = alexError ("Error occurred at line " ++ (show (getRow (position tk))) ++ " column " ++ (show (getCol (position tk))) ++ ": invalid token")

fatalError :: (Show a1, Show a) => [Char] -> a -> a1 -> t
fatalError s l c = error ("Error at line " ++ (show l) ++ " column " ++ (show c) ++ ": " ++ s)

fatalError_2 :: [Char] -> t
fatalError_2 s = error ("Error:  " ++ s)

parse :: String -> Either String [Declaration]
parse s = runAlex s oLikeParse

main = do
  inStr <- getContents
  let result = parse inStr
  putStrLn ("result: " ++ ( either show show result ))
}
