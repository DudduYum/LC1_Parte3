module TypeCheck
 where

  import ErroreType
  import qualified Data.Set as Set
  import qualified Data.Map as Map

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
					isParameter :: Bool,
					isPassedByReference :: Bool } deriving (Show, Eq)


  data AttributeType 	= Simple SimpleType
  					| UnsizedArray AttributeType
  					| Array Integer AttributeType
  					deriving (Show, Eq)


  data SimpleType = String
  	| Float
  	| Char
  	| Integer
  	| Boolean
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

  data Procedure = Procedure { 	procedureName :: String,
			attributes :: [Attribute],
			procedureProcedures :: [Procedure],
			returnType :: Maybe AttributeType } deriving (Show, Eq)

  data Enviroment = Envi ([Attribute], [Procedure] )

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
  								isParameter = False,
  								isPassedByReference = False }


  createEnviroment :: Enviroment
  createEnviroment = Envi ([],[])

  envi = createEnviroment

  -- testRosty :: m a -> m b



  checkBasicOperation :: Monad m => BasicOperation -> t -> m AttributeType
  checkBasicOperation (OP_add v u) env = if c /= b
      then fail ("Can't add " ++ (show c) ++ " to " ++ (show b))
      else {-return-} (solveType (c , b))
    where
      c = (attributeType v)
      b = (attributeType u)
  --
  -- genereteMesege errStr (Err a) = return (Simple Float)

-- per risolvere la compatibilia' di tipi
  solveType attType@( a , b ) = let
    swapedTypes = typeSwap attType
    in case attType of
      ( (Simple Float) , (Simple Integer) )  -> return (Simple Float)
      ( (Simple Float) , (Simple Boolean) )  -> fail ("Float" ++ " Boolean")
      ( (Simple Float) , (Simple Char) )     -> fail ("Float" ++ " Char")
      ( (Simple Float) , (Simple String) )   -> fail ("Float" ++ " String")
      ( (Simple Integer) , (Simple Boolean) )-> fail ("Integer" ++ " Boolean")
      ( (Simple Integer) , (Simple Char) )   -> fail ("Integer" ++ " Char")
      ( (Simple Integer) , (Simple String) ) -> fail ("Integer" ++ " String")
      ( (Simple Boolean) , (Simple Char) )   -> fail ("Boolean" ++ " Char")
      ( (Simple Boolean) , (Simple String) ) -> fail ("Boolean" ++ " String")
      ( (Simple String) , (Simple Char) )    -> return (Simple String)



  typeSwap (a , b) = let
    wA = typeWeith a
    wB = typeWeith b
    in if wA <= wB
      then (b , a)
      else (a , b)

  typeWeith (Simple a) = case a of
    Float -> 5
    Integer -> 4
    Boolean -> 3
    String -> 2
    Char -> 1
