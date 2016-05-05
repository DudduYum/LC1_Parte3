 
 
-- Qualche prova per capirsi, sul pretty printer
 
module Pretty-printer where

import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass

-- Qui bisognerà implementare la struttura dati utilizzata dal parser

data Program a =        Procedure ( Program a )
               |        Attribute a deriving (Show)
               
               
-- Si procede poi a definire come sarà l'indentazione per casi
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 1 ( pPrint p )]
                                    

{- con Procedure e Attribute che possono essere uno tra i seguenti:
 "INTEGER"                 
 "REAL"                    
 "BOOLEAN"                 
 "CHAR"                    
 "SET"                     
 "ARRAY"                   
 "OF"                      
 "POINTER TO"              
 "PROCEDURE"               
 "BEGIN"                  
 "END"                     
 "VAR"                    
 "CONST"                   
 "TRUE"                    
 "FALSE"                   
 "IF"                      
 "ELSIF"                   
 "ELSE"                    
 "THEN"                    
 "CASE"                    
 "WHILE"                   
 "DO"                      
 "REPEAT"                  
 "UNTIL"                   
 "LOOP"                    
 "EXIT"                    
 "RETURN"                 
 "BREAK"                   
 "CONTINUE"                
-}





