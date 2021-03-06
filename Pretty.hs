 
 
-- Qualche prova per capirsi, sul pretty printer
 
module Pretty where

import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJ

-- Qui bisognerà implementare la struttura dati utilizzata dal parser
{-
data Program a =        Procedure ( Program a )
               |        Attribute a deriving (Show)
               
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 2 ( pPrint p )]
                                    -}
 {-                                   
                                    
data Program a =            Procedure ( Program a )
                   |        Attribute a
		   |	    AttributeType a
		   |	    SimpleType a 
		   |	    BasicOperation a
		   |	    Operation a
		   |	    DeclarationType a
		   |	    Declaration a deriving (Show)
               
               
-- Si procede poi a definire come sarà l'indentazione per casi ( ricorsivamente )
-- Si prevede una struttura ad albero necessaria per eventuali annidamenti
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( AttributeType a) = text "VARTYPE: " <> pPrint a
    pPrint ( SimpleType a) = text "TYPE: " <> pPrint a
    pPrint ( BasicOperation a ) = text "BASOP: " <> pPrint a
    pPrint ( Operation a ) = text "OPER: " <> pPrint a
    pPrint ( DeclarationType a) = text "DECTYPE: " <> pPrint a
    pPrint ( Declaration a) = test "DECLA: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 1 ( pPrint p )]
               
               -}
        
        
        
-- Manca da sistemare l'indentazione nelle instances delle procedure ( IF, WHILE ecc... )
        
        
 class PrettyPrinter a where       
        
  toDoc :: a -> Doc

  prettyPrint :: a -> String
  prettyPrint = toString

  toString :: a -> String
  toStringS :: a -> ShowS
  toString = show . toDoc
  toStringS = shows . toDoc

-- lunghezza tabulazione dell'indentazione

tabulation = 2

data AttributeType 	= Simple SimpleType
                        | UnsizedArray AttributeType 
                        | Array Integer AttributeType
                        deriving (Show, Eq)
instance PrettyPrinter AttributeType where
    toDoc Simple SimpleType = text
    toDoc UnsizedArray AttributeType = text 
    toDoc Array Integer AttributeType = text
                        

data SimpleType = ( String n )
	| Float
	| Char
	| Integer
	| Boolean
	| Pointer SimpleType
        | ( Array n SimpleType )
	deriving (Show, Eq)
instance PrettyPrinter SimpleType where
    toDoc ( String n ) = text "ARRAY" <> text n <> "OF CHAR"
    toDoc Float = text "REAL"
    toDoc Char = text "CHAR"
    toDoc Integer = text "INTEGER"
    toDoc Boolean = text "BOOLEAN"
    toDoc ( Pointer SimpleType ) = text "POINTER OF" <+> toDoc SimpleType
    toDoc ( Array n SimpleType ) = text "ARRAY" <> text n <> text "OF" <+> toDoc SimpleType

    
	
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
instance PrettyPrinter BasicOperation where
    toDoc OP_sub Attribute Attribute = toDoc Attribute <+> text "-" <+> toDoc Attribute
    toDoc OP_iden_add Attribute = toDoc Attribute <+> text "+" <+> toDoc Attribute       -- Non sicuro
    toDoc OP_iden_sub Attribute = toDoc Attribute <+> text "-" <+> toDoc Attribute       -- Non sicuro                           
    toDoc OP_div Attribute Attribute = toDoc Attribute <+> text "DIV" <+> toDoc Attribute
    toDoc OP_mul Attribute Attribute = toDoc Attribute <+> text "*" <+> toDoc Attribute
    toDoc OP_quot Attribute Attribute = toDoc Attribute <+> text "/" <+> toDoc Attribute
    toDoc OP_mod Attribute Attribute = toDoc Attribute <+> text "MOD" <+> toDoc Attribute
    toDoc OP_and Attribute Attribute = toDoc Attribute <+> text "AND" <+> toDoc Attribute
    toDoc OP_or Attribute Attribute = toDoc Attribute <+> text "OR" <+> toDoc Attribute
    toDoc OP_min Attribute Attribute = toDoc Attribute <+> text "<" <+> toDoc Attribute
    toDoc OP_mineq Attribute Attribute = toDoc Attribute <+> text "<=" <+> toDoc Attribute
    toDoc OP_maj Attribute Attribute = toDoc Attribute <+> text ">" <+> toDoc Attribute
    toDoc OP_majeq Attribute Attribute = toDoc Attribute <+> text ">=" <+> toDoc Attribute
    toDoc OP_eq Attribute Attribute = toDoc Attribute <+> text "=" <+> toDoc Attribute
    toDoc OP_neq Attribute Attribute = toDoc Attribute <+> text "!=" <+> toDoc Attribute
                                        
                                        
                                        
                                        
data Operation 	= OP_Assignment Attribute Attribute
				| OP_ProcedureCall Attribute [Attribute]
				| OP_Exit
				| OP_Continue
				| OP_Break
				| OP_Return (Maybe Attribute)
				| OP_If (Attribute, [Operation])
				| OP_If_Else (Attribute, [Operation], [Operation])
				| OP_If_Elsif [(Attribute, [Operation])]
				| OP_If_Elsif_Else ([(Attribute, [Operation])], [Operation])
				| OP_While Attribute [Operation]
				| OP_Repeat [Operation] Attribute
				| OP_Loop [Operation]
				deriving (Show, Eq)
instance PrettyPrinter Operation where
    toDoc OP_Assignment Attribute Attribute = toDoc Attribute <+> text ":=" <+> toDoc Attribute
    toDoc OP_ProcedureCall Attribute [Attribute] = text "PROCEDURE" <+> text 
    toDoc OP_Exit = text "EXIT"
    toDoc OP_Continue = text "CONTINUE"
    toDoc OP_Break = text "BREAK"
    toDoc OP_Return (Maybe Attribute) = text "RETURN" (Maybe <+> toDoc Attribute)               -- Non sicuro
    toDoc OP_If (Attribute, [Operation]) = text "IF" <+> toDoc Attribute <+> toDoc Operation
    toDoc OP_If_Else (Attribute, [Operation], [Operation]) = text "IF ELSE" <+> toDoc Attribute <+> toDoc Operation <+> toDoc Operation
    toDoc OP_If_Elsif [(Attribute, [Operation])] = text "IF ELSE IF" <+> toDoc Attribute <+> toDoc Operation
    toDoc OP_If_Elsif_Else ([(Attribute, [Operation])], [Operation]) = text "IF ELSE IF ELSE" <+> toDoc Attribute <+> toDoc Operation <+> toDoc Operation
    toDoc OP_While Attribute [Operation] = text "WHILE" <+> toDoc Attribute <+> toDoc Operation
    toDoc OP_Repeat [Operation] Attribute = text "REPEAT" <+> toDoc Operation <+> toDoc Attribute
    toDoc OP_Loop [Operation] = text "LOOP" <+> toDoc Operation
    
    
				
				
data DeclarationType 	= DT_Variable
						| DT_Constant
						| DT_Procedure
						| DT_Operation
						deriving (Show, Eq)
instance PrettyPrinter DeclarationType where
    toDoc DT_Variable = text
    toDoc DT_Constant = text
    toDoc DT_Procedure = text
    toDoc DT_Operation = text

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
