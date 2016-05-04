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

data ProcedureStack = ProcedureStack { levels :: [Procedure] , deaph :: [Int] }deriving(Show)

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

defaultProcedureStack = ProcedureStack {
 								levels = [] ,
								deaph = [0] }

-- serve per creare lo stack di procedure (per gestire la profondita di chiamate)
pushProcedureToStack :: Procedure -> Int -> ProcedureStack -> ProcedureStack
pushProcedureToStack p newDepth (ProcedureStack { levels = l , deaph = oldDeaph} ) = ProcedureStack { levels = (p : l) ,deaph = (newDepth : oldDeaph)}

lookProcedureToStack :: ProcedureStack -> (Maybe Procedure , Int)
lookProcedureToStack (ProcedureStack { levels = (x:xs) , deaph = (y:ys) }) = (Just x , y)
lookProcedureToStack (ProcedureStack { levels = []  }) = (Nothing , 0)

addAttributeToProcedure :: Procedure -> Attribute -> Procedure
addAttributeToProcedure proc att = Procedure { 	procedureName = (procedureName proc),
												attributes = (attributes proc) ++ [att],
												procedureProcedures = (procedureProcedures proc) }


-- lookUpForDefinition ::
-- lookUpForDefinition atr index (x:xs) = 

changeAttributeType :: Attribute -> AttributeType -> Attribute
changeAttributeType attr attrType = attr { attributeType = attrType }

attributesSameType :: Attribute -> Attribute -> Bool
attributesSameType a1 a2 = (attributeType a1) == (attributeType a2)

