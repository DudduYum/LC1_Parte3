module OberonTools where

data AttributeType 	= Simple SimpleType
					| Array [ Integer ] AttributeType
					deriving (Show, Eq)

data SimpleType = String
	| Float
	| Char
	| Integer
	| Boolean
	deriving (Show, Eq)


data Attribute = Attribute {	attributeName :: String,
								attributeType :: AttributeType,
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
								isConstant :: Bool,
								isPassedByReference :: Bool } deriving (Show, Eq)

data Procedure = Procedure { 	procedureName :: String,
								attributes :: [Attribute],
								procedureProcedures :: [Procedure] } deriving (Show, Eq)

data Program = Program { programProcedures :: [Procedure] } deriving (Show)

data DeclarationType 	= DT_Variable
						| DT_Constant
						| DT_Procedure
						deriving (Show, Eq)

data Declaration = Declaration {	declarationType 	:: DeclarationType,
									attributeDeclared	:: Maybe Attribute,
									procedureDeclared	:: Maybe Procedure } deriving (Show)

defaultAttribute = Attribute {	attributeName = "",
								attributeType =Simple Integer,
								-- attributeType = Integer,
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
								isConstant = False,
								isPassedByReference = False }

defaultProcedure = Procedure { 	procedureName = "",
								attributes = [],
								procedureProcedures = [] }

defaultDeclaration = Declaration { 	declarationType = DT_Variable,
									attributeDeclared = Nothing,
									procedureDeclared = Nothing }

createProcedure :: String -> [Procedure] -> [Procedure]
createProcedure childName stack = let
	child = defaultProcedure { procedureName = childName}
	in pushProcedureToStack child stack

-- serve per creare lo stack di procedure (per gestire la profondita di chiamate)
pushProcedureToStack :: Procedure -> [Procedure] -> [Procedure]
pushProcedureToStack p procedures = p:procedures


-- lookProcedureToStack :: [Procedure]  -> Maybe Procedure
-- lookProcedureToStack (x:xs) = Just x
-- lookProcedureToStack []   = Nothing

-- lookProcedureToStack :: [Procedure]  -> Procedure
-- lookProcedureToStack (x:xs) = x

lookProcedureToStack :: [Procedure] -> Maybe Procedure
lookProcedureToStack (x:xs) = Just x
lookProcedureToStack []   = Nothing


-- popProcedureFromStack :: [Procedure] -> [Procedure]
-- popProcedureFromStack (x:y:xs) = let
-- 					listaProcedureOld = procedureProcedures y
-- 					in let
-- 						listaProcedureNew = (x : listaProcedureOld)
-- 							in ((Procedure { procedureName = (procedureName y) , attributes = (attributes y) , procedureProcedures = listaProcedureNew}) : xs)

popProcedureFromStack :: [Procedure] -> [Procedure]
popProcedureFromStack (x:y:xs) = ((Procedure { procedureName = (procedureName y) , attributes = (attributes y) , procedureProcedures = listaProcedureNew}) : xs)
	where
		listaProcedureNew = (x : listaProcedureOld)
		listaProcedureOld = procedureProcedures y

addAttributeToProcedure :: Procedure -> Maybe Attribute -> Procedure
addAttributeToProcedure proc (Just att) = Procedure { 	procedureName = (procedureName proc),
														attributes = (attributes proc) ++ [att],
														procedureProcedures = (procedureProcedures proc) }

addProcedureToProcedure :: Procedure -> Maybe Procedure -> Procedure
addProcedureToProcedure procDest (Just procToAdd) 	= Procedure { 	procedureName = (procedureName procDest),
																	attributes = (attributes procDest),
																	procedureProcedures = (procedureProcedures procDest) ++ [procToAdd] }

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
											else
												if (procedureDeclared decl) == Nothing then
													addBodyToProcedure procDest (tail declList)
												else
													addBodyToProcedure (addProcedureToProcedure procDest (procedureDeclared decl)) (tail declList)

createVariablesDefinitionsOfType :: [String] -> AttributeType -> [Declaration]
createVariablesDefinitionsOfType namesList t = map (\x -> defaultDeclaration { declarationType = DT_Variable, attributeDeclared = Just (defaultAttribute {attributeName = x, attributeType = t})} ) namesList


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

attributesSameType :: Attribute -> Attribute -> Bool
attributesSameType a1 a2 = (attributeType a1) == (attributeType a2)

--addBodyToProcedure proc Nothing = Just proc
--addBodyToProcedure proc body = Just (addBodyToProcedure proc (defaultAttribute {attributeName = body}))
