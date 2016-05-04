module OberonTools where

data AttributeType 	= String
					| Float
					| Char
					| Integer
					| StringArray
					| FloatArray
					| CharArray
					| IntegerArray
					deriving (Show, Eq)

data Attribute = Attribute {	attributeName :: String,
								attributeType :: AttributeType,
								stringValue :: String,
								floatValue :: Float,
								integerValue :: Integer,
								charValue :: Char,
								stringArrayValue :: [String],
								floatArrayValue :: [Float],
								integerArrayValue :: [Integer],
								charArrayValue :: [Char],
								isConstant :: Bool,
								isPassedByReference :: Bool } deriving (Show)

data Procedure = Procedure { 	procedureName :: String,
								attributes :: [Attribute],
								procedureProcedures :: [Procedure] } deriving (Show)

-- data [Procedure]  = [Procedure]  { sProcedures :: [Procedure]  }deriving(Show)

data Program = Program { programProcedures :: [Procedure] } deriving (Show)

defaultAttribute = Attribute {	attributeName = "",
								attributeType = Integer,
								stringValue = "",
								floatValue = 0.0,
								integerValue = 0,
								charValue = ' ',
								stringArrayValue = [],
								floatArrayValue = [],
								integerArrayValue = [],
								charArrayValue = [],
								isConstant = False,
								isPassedByReference = False }

defaultProcedure = Procedure { 	procedureName = "",
								attributes = [],
								procedureProcedures = [] }

-- default[Procedure]  = [Procedure]  {
--  								sProcedures = []
-- 								}

createProcedure :: String -> [Procedure]  -> [Procedure]
createProcedure childName stack = let
	child = defaultProcedure { procedureName = childName}
	in pushProcedureToStack child stack

-- serve per creare lo stack di procedure (per gestire la profondita di chiamate)
pushProcedureToStack :: Procedure -> [Procedure]  -> [Procedure]
pushProcedureToStack p procedures = p:procedures

lookProcedureToStack :: [Procedure]  -> Maybe Procedure
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


addAttributeToProcedure :: Procedure -> Attribute -> Procedure
addAttributeToProcedure proc att = Procedure { 	procedureName = (procedureName proc),
												attributes = (attributes proc) ++ [att],
												procedureProcedures = (procedureProcedures proc) }



changeAttributeType :: Attribute -> AttributeType -> Attribute
changeAttributeType attr attrType = attr { attributeType = attrType }

attributesSameType :: Attribute -> Attribute -> Bool
attributesSameType a1 a2 = (attributeType a1) == (attributeType a2)
