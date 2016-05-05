 
 
-- Qualche prova per capirsi, sul pretty printer
 
module Pretty-printer where

import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass ( se necessaria )

-- Qui bisognerà implementare la struttura dati utilizzata dal parser

data Program a =        Procedure ( Program a )
               |        Attribute a deriving (Show)
               
               
-- Si procede poi a definire come sarà l'indentazione per casi ( ricorsivamente )
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 1 ( pPrint p )]
                                    

{- con Procedure e Attribute che possono essere uno tra i seguenti:       ( A > attributes, P > procedures )
 "INTEGER"        A         
 "REAL"           A         
 "BOOLEAN"        A         
 "CHAR"           A         
 "SET"            P         
 "ARRAY"          A         
 "POINTER TO"     A         
 "PROCEDURE"      P         
 "BEGIN"          P        
 "END"            P         
 "VAR"            A        
 "CONST"          A        
 "IF"             P         
 "ELSIF"          P         
 "ELSE"           P         
 "THEN"           P         
 "CASE"           P         
 "WHILE"          P         
 "DO"             P         
 "REPEAT"         P         
 "UNTIL"          P         
 "LOOP"           P         
 "EXIT"           P         
 "RETURN"         P        
 "BREAK"          P         
 "CONTINUE"       P         
-}

-- Infine si fornisce pure qualche esempio ai fini della comprensione

{- Esempio di indentazione:

PROCEDURE p1;
	    PROCEDURE p12;
	 	       PROCEDURE p123;
		       END p123
     	    END p12;
	    PROCEDURE p22;
	         PROCEDURE p1234;
	         END p1234
	    END p22
END p1

-}
