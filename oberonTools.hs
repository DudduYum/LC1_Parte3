module OberonTools where

data AttributeType 	= Simple SimpleType
					| UnsizedArray AttributeType 	-- Questa tipologia di array, senza definizione della lunghezza, è usata per la dichiarazione dei parametri formali di una procedura
					| Array Integer AttributeType
--					| Pointer AttributeType
					deriving (Show, Eq)

data SimpleType = String
	| Float
	| Char
	| Integer
	| Boolean
	| Name
	| Unknown 				-- Se un attributo e' di tipo Unknown significa che per calcolarlo e' necessario utilizzare una variabile, o una costante, e non si sa ancora di che tipo e'
	| OperationResult
	deriving (Show, Eq)

data BasicOperation = OP_add Attribute Attribute
					| OP_sub Attribute Attribute
					| OP_iden_add Attribute
					| OP_iden_sub Attribute
					| OP_div Attribute Attribute
					| OP_mul Attribute Attribute
					| OP_quot Attribute Attribute
					| OP_mod Attribute Attribute
					| OP_and Attribute Attribute
					| OP_or Attribute Attribute
					| OP_min Attribute Attribute
					| OP_mineq Attribute Attribute
					| OP_maj Attribute Attribute
					| OP_majeq Attribute Attribute
					| OP_eq Attribute Attribute
					| OP_neq Attribute Attribute
					deriving (Show, Eq)

data Operation 	= OP_Assignment Attribute Attribute
				| OP_ProcedureCall Attribute [Attribute]
				| OP_Exit
				| OP_Continue
				| OP_Break
				| OP_Return (Maybe Attribute)
				| OP_If Attribute [Operation]
				| OP_If_Else Attribute [Operation] [Operation]
				| OP_While Attribute [Operation]
				deriving (Show, Eq)

data Attribute = Attribute {	attributeType :: AttributeType,				-- Indica il tipo di attributo (float, integer, ecc)
								nameValue :: String,						-- Usato dalla regola di produzione di 'designator' per indicare il nome di un varibile, costante o procedura
								attributeName :: String,					-- Indica il nome dell'attributo. Serve per identificare le variabili e le costanti.
								operationResultValue :: Maybe Attribute,	-- Serve per indicare il valore di ritorno di una operazione
								stringValue :: String,						-- Memorizza il valore stringa per gli attributi di tipo STRING
								floatValue :: Float,						-- Memorizza il valore float per gli attributi di tipo FLOAT
								integerValue :: Integer,					-- Memorizza il valore intero per gli attributi di tipo INTEGER
								charValue :: Char,							-- Memorizza il valore carattere per gli attributi di tipo CHAR
								booleanValue :: Bool,						-- Memorizza il valore booleano per gli attributi di tipo BOOLEAN
								stringArrayValue :: [String],				-- Memorizza il valore array di stringhe per gli attributi di tipo ARRAY n OF STRING
								floatArrayValue :: [Float], 				-- Memorizza il valore array di float per gli attributi di tipo ARRAY n OF FLOAT
								integerArrayValue :: [Integer],				-- Memorizza il valore array di interi per gli attributi di tipo ARRAY n OF INTEGER
								charArrayValue :: [Char],					-- Memorizza il valore array di caratteri per gli attributi di tipo ARRAY n OF CHAR
								booleanArrayValue :: [Bool],				-- Memorizza il valore array di booleani per gli attributi di tipo ARRAY n OF BOOLEAN
								basicOperation :: Maybe BasicOperation, 	-- Memorizza l'operazione da eseguire per gli attributi di tipo Unknown
								procCallParams :: [Attribute],				-- Memorizza la lista di parametri per la chiamata di una procedura. Questo e' valido quando isProcedureCall e' True e attributeType e' Name
								isProcedureCall :: Bool,					-- Indica se questo attributo e' in realta' una chiamata ad una procedura. Questo puo' essere valido quando attributeType e' Name
								isConstant :: Bool,							-- Indica se questo attributo e' una costante o una variabile. Serve nel caso la costante o variabile sia definita all'interno della sezione di chiarazione di una procedura
								isParameter :: Bool, 						-- Serve per sapere se questo attributo e' un parametro di una procedura
								isPassedByReference :: Bool 				-- Serve per sapere, nel caso questo attributo sia un valore passato come argomento a una procedura, se l'argomento e' passato per riferimento
							} deriving (Show, Eq)

data Procedure = Procedure { 	procedureName 		:: String,
								attributes 			:: [Attribute],
								procedureProcedures :: [Procedure],
								procedureOperations :: [Operation],
								returnType 			:: Maybe AttributeType } deriving (Show, Eq)

data DeclarationType 	= DT_Variable
						| DT_Constant
						| DT_Procedure
						| DT_Operation
						deriving (Show, Eq)

data Declaration = Declaration {	declarationType 	:: DeclarationType,
									attributeDeclared	:: Maybe Attribute,
									procedureDeclared	:: Maybe Procedure,
									operationDeclared 	:: Maybe Operation } deriving (Show)

defaultAttribute = Attribute {	attributeName = "",
								attributeType = Simple Integer,
								nameValue = "",
								operationResultValue = Nothing,
								stringValue = "",
								floatValue = 0.0,
								integerValue = 0,
								charValue = ' ',
								booleanValue = False,
								stringArrayValue = [],
								floatArrayValue = [],
								integerArrayValue = [],
								charArrayValue = [],
								booleanArrayValue = [],
								basicOperation = Nothing,
								procCallParams = [],
								isProcedureCall = False,
								isConstant = False,
								isParameter = False,
								isPassedByReference = False }

defaultProcedure = Procedure { 	procedureName = "",
								attributes = [],
								procedureProcedures = [],
								procedureOperations = [],
								returnType = Nothing }

defaultDeclaration = Declaration { 	declarationType = DT_Variable,
									attributeDeclared = Nothing,
									procedureDeclared = Nothing,
									operationDeclared = Nothing }

addAttributeToProcedure :: Procedure -> Maybe Attribute -> Procedure
addAttributeToProcedure procDest (Just attToAdd) 	= Procedure { 	procedureName = (procedureName procDest),
																	attributes = (attributes procDest) ++ [attToAdd],
																	procedureProcedures = (procedureProcedures procDest),
																	procedureOperations = (procedureOperations procDest),
																	returnType = (returnType procDest) }

addProcedureToProcedure :: Procedure -> Maybe Procedure -> Procedure
addProcedureToProcedure procDest (Just procToAdd) 	= Procedure { 	procedureName = (procedureName procDest),
																	attributes = (attributes procDest),
																	procedureProcedures = (procedureProcedures procDest) ++ [procToAdd],
																	procedureOperations = (procedureOperations procDest),
																	returnType = (returnType procDest) }

addOperationToProcedure :: Procedure -> Maybe Operation -> Procedure
addOperationToProcedure procDest (Just operToAdd) 	= Procedure { 	procedureName = (procedureName procDest),
																	attributes = (attributes procDest),
																	procedureProcedures = (procedureProcedures procDest),
																	procedureOperations = (procedureOperations procDest) ++ [operToAdd],
																	returnType = (returnType procDest) }

declarationListToOperationList :: [Declaration] -> [Operation]
declarationListToOperationList [] = []
declarationListToOperationList declList = do
											let decl = head declList
											let declType = declarationType decl
											
											if declType == DT_Operation then
												if (operationDeclared decl) == Nothing then
													[]++(declarationListToOperationList (tail declList))
												else
													(getMaybeValue (operationDeclared decl)):(declarationListToOperationList (tail declList))
											else
												[]++(declarationListToOperationList (tail declList))

addBodyToProcedure :: Procedure -> [Declaration] -> Procedure
addBodyToProcedure procDest []			= procDest
addBodyToProcedure procDest declList	= do
											let decl = head declList
											let declType = declarationType decl

											if declType == DT_Variable || declType == DT_Constant then
												if (attributeDeclared decl) == Nothing then
													addBodyToProcedure procDest (tail declList)
												else
													addBodyToProcedure (addAttributeToProcedure procDest (attributeDeclared decl)) (tail declList)
											else if declType == DT_Operation then
												if (operationDeclared decl) == Nothing then
													addBodyToProcedure procDest (tail declList)
												else
													addBodyToProcedure (addOperationToProcedure procDest (operationDeclared decl)) (tail declList)
											else
												if (procedureDeclared decl) == Nothing then
													addBodyToProcedure procDest (tail declList)
												else
													addBodyToProcedure (addProcedureToProcedure procDest (procedureDeclared decl)) (tail declList)

addParametersToProcedure :: Procedure -> [Attribute] -> Procedure
addParametersToProcedure procDest []		= procDest
addParametersToProcedure procDest attribs	= addParametersToProcedure (addAttributeToProcedure procDest (Just (head attribs))) (tail attribs)

createVariablesDefinitionsOfType :: [String] -> AttributeType -> [Declaration]
createVariablesDefinitionsOfType namesList t = map (\x -> defaultDeclaration { declarationType = DT_Variable, attributeDeclared = Just (defaultAttribute {attributeName = x, attributeType = t})} ) namesList

createProcedureParametersByValueDefinitionsOfType :: [String] -> AttributeType -> [Attribute]
createProcedureParametersByValueDefinitionsOfType namesList t = map (\x -> defaultAttribute {attributeName = x, attributeType = t, isParameter = True}) namesList

createProcedureParametersByReferenceDefinitionsOfType :: [String] -> AttributeType -> [Attribute]
createProcedureParametersByReferenceDefinitionsOfType namesList t = map (\x -> defaultAttribute {attributeName = x, attributeType = t, isParameter = True, isPassedByReference = True}) namesList

createMultidimensionalArrayOfType :: [Integer] -> AttributeType -> AttributeType
createMultidimensionalArrayOfType [x] typ = Array x typ
createMultidimensionalArrayOfType lst typ = Array (head lst) (createMultidimensionalArrayOfType (tail lst) typ)

getOperationResult :: Maybe Attribute -> Maybe Attribute
getOperationResult Nothing 		= 	Nothing
getOperationResult (Just att) 	= 	let attType = attributeType att
									in
										if attType == Simple OperationResult then
											getOperationResult (operationResultValue att)
										else if attType == Simple Integer then
											Just defaultAttribute { attributeType = attType, integerValue = (integerValue att) }
										else if attType == Simple Float then
											Just defaultAttribute { attributeType = attType, floatValue = (floatValue att) }
										else if attType == Simple Char then
											Just defaultAttribute { attributeType = attType, charValue = (charValue att) }
										else if attType == Simple String then
											Just defaultAttribute { attributeType = attType, stringValue = (stringValue att) }
										else if attType == Simple Boolean then
											Just defaultAttribute { attributeType = attType, booleanValue = (booleanValue att) }
										else if attType == Simple Name || attType == Simple Unknown then
											Just att
										else 
											do
												let arrType = getMaybeValue (getArrayType attType)

												if arrType == Simple Integer then
													Just defaultAttribute { attributeType = attType, integerArrayValue = (integerArrayValue att) }
												else if arrType == Simple Float then
													Just defaultAttribute { attributeType = attType, floatArrayValue = (floatArrayValue att) }
												else if arrType == Simple Char then
													Just defaultAttribute { attributeType = attType, charArrayValue = (charArrayValue att) }
												else if arrType == Simple String then
													Just defaultAttribute { attributeType = attType, stringArrayValue = (stringArrayValue att) }
												else if arrType == Simple Boolean then
													Just defaultAttribute { attributeType = attType, booleanArrayValue = (booleanArrayValue att) }
												else
													Nothing

getArraySize :: AttributeType -> Integer
getArraySize (Array a _) = a
getArraySize _ = 0

getArrayType :: AttributeType -> Maybe AttributeType
getArrayType (Array _ b) = Just b
getArrayType (Simple _) = Nothing
getArrayType (UnsizedArray a) = Just a

getMaybeValue :: Maybe a -> a
getMaybeValue (Just v) = v

isSameType :: AttributeType -> AttributeType -> Bool
isSameType a b = a == b

updateStackAttr :: [Procedure] -> Procedure -> [Procedure]
updateStackAttr (x:xs) updatedProc = updatedProc : xs

changeAttributeType :: Attribute -> AttributeType -> Attribute
changeAttributeType attr attrType = attr { attributeType = attrType }

listElementIsLessOrEqualZero :: [Integer] -> Bool
listElementIsLessOrEqualZero [] = False
listElementIsLessOrEqualZero ls = do
									let elem = head ls

									if elem <= 0 then
										True
									else
										listElementIsLessOrEqualZero (tail ls)

attributeIsOfType :: Attribute -> AttributeType -> Bool
attributeIsOfType att typ = (attributeType att) == typ

stringsEqual :: String -> String -> Bool
stringsEqual s1 s2 = do
						let l1 = (length s1)
						let l2 = (length s2)

						if l1 < l2 || l1 > l2 then
							False
						else
							stringsEqualHelper s1 s2

stringsEqualHelper :: String -> String -> Bool
stringsEqualHelper [] [] =	True
stringsEqualHelper s1 s2 = 	do
								if (head s1) == (head s2) then
									stringsEqualHelper (tail s1) (tail s2)
								else
									False

attributesSameType :: Attribute -> Attribute -> Bool
attributesSameType a1 a2 = (attributeType a1) == (attributeType a2)