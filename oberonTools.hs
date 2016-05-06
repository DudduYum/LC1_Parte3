module OberonTools where

data AttributeType 	= Simple SimpleType
					| UnsizedArray AttributeType
					| Array Integer AttributeType
--					| Pointer AttributeType
					deriving (Show, Eq)

data SimpleType = String
	| Float
	| Char
	| Integer
	| Boolean
	| Name
	| Unknown
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
--				| OP_ProcedureCall Procedure [Attribute]
--				| OP_If Attribute [Operation]
				deriving (Show, Eq)

data Attribute = Attribute {	attributeType :: AttributeType,
								nameValue :: String,	-- Usato dalla regola di produzione di 'designator' per indicare il nome di un varibile, costante o procedure
								attributeName :: String,
								operationResultValue :: Maybe Attribute,
								stringValue :: String,
								floatValue :: Float,
								integerValue :: Integer,
								charValue :: Char,
								booleanValue :: Bool,
								stringArrayValue :: [String],
								floatArrayValue :: [Float],
								integerArrayValue :: [Integer],
								charArrayValue :: [Char],
								booleanArrayValue :: [Bool],
								basicOperation :: Maybe BasicOperation,
								isBasicOperationResult :: Bool,
								isConstant :: Bool,
								isParameter :: Bool,
								isPassedByReference :: Bool } deriving (Show, Eq)

data Procedure = Procedure { 	procedureName 		:: String,
								attributes 			:: [Attribute],
								procedureProcedures :: [Procedure],
								procedureOperations :: [Operation],
								returnType 			:: Maybe AttributeType } deriving (Show, Eq)

data Program = Program { programProcedures :: [Procedure] } deriving (Show)

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
								isBasicOperationResult = False,
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