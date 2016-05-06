 
 
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
                                    
-- nest si occupa dell'indentazione, dato il numero di volte da fare; con quello si può tenere conto di indentazioni precedenti, per esempio nel caso di funzioni/procedure annidate

{- con Procedure e Attribute che possono essere uno tra i seguenti:       ( A > attributes, P > procedures )
 "PROCEDURE"      P         
 "BEGIN"          P        
 "END"            P         
 "VAR"            A        
 "IF"             P         
 "ELSIF"          P         
 "ELSE"           P         
 "CASE"           P         
 "WHILE"          P    
 "LOOP"		  P
 "REPEAT"         P       
 "EXIT"           P         
 "RETURN"         P        
 "BREAK"          P         
 "CONTINUE"       P   
 
 Per gli attributi, bisogna tenere conto di eventuali assegnamenti alle variabili
 
-}

-- Infine si fornisce pure qualche esempio ai fini della comprensione

{- Esempio di indentazione:

PROCEDURE p1;
	    PROCEDURE p12;
	 	       PROCEDURE p123;
		       END p123;
     	    END p12;
	    PROCEDURE p22;
	               PROCEDURE p1234;
	               END p1234;
	    END p22;
END p1;

-}
