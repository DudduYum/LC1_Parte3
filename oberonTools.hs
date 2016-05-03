module OberonTools where

data AttributeType 	= String
					| Float
					| Char
					| Integer
					| StringArray
					| FloatArray
					| CharArray
					| IntegerArray
					deriving (Show)

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

data ProcedureStack = ProcedureStack { levels :: [Procedure] }deriving(Show)

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

pushProcedureToStack :: Procedure -> ProcedureStack -> ProcedureStack
pushProcedureToStack p (ProcedureStack { levels = l } ) = ProcedureStack { levels = (p : l) }

lookProcedureToStack :: ProcedureStack -> Maybe Procedure
lookProcedureToStack (ProcedureStack { levels = (x:xs) }) = Just x
lookProcedureToStack (ProcedureStack { levels = [] }) = Nothing

addAttributeToProcedure :: Procedure -> Attribute -> Procedure
addAttributeToProcedure proc att = Procedure { 	procedureName = (procedureName proc),
												attributes = (attributes proc) ++ [att],
												procedureProcedures = (procedureProcedures proc) }