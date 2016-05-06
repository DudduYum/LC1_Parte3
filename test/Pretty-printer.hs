 
 
-- Qualche prova per capirsi, sul pretty printer
 
module Pretty-printer where

import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass ( se necessaria )

-- Qui bisognerà implementare la struttura dati utilizzata dal parser

data Program a =        Procedure ( Program a )
               |        Attribute a deriving (Show)
               
               
-- Si procede poi a definire come sarà l'indentazione per casi ( ricorsivamente )
-- Si prevede una struttura ad albero necessaria per eventuali annidamenti
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 1 ( pPrint p )]
                                    
-- nest si occupa dell'indentazione, dato il numero di volte da fare; con quello si può tenere conto di indentazioni precedenti, per esempio nel caso di funzioni/procedure annidate

data Program a =	Procedure a
		|	Begin a
		|	End a
		|	Var a
		|	a			-- assegnamento, da controllare
		|	If a
		|	Elseif a
		|	Else a
		|	Case a
		|	While a
		|	Loop a
		|	Repeat a
		|	Return a
		|	Break a
		|	Return a
		|	Program ( Program a ) deriving (Show)
		
k = 0		-- variabile necessaria per il numero di indentazioni
		
instance Pretty a => Pretty ( Program a ) where
	pPrint ( Procedure a ) = text "PROCEDURE: " <> pPrint a
	pPrint ( Begin a ) = text "BEGIN: " <> pPrint a
	pPrint ( End a ) = text "END: " <> pPrint a 	-- Aggiungere k - 1
	pPrint ( Var a ) = text "VAR: " <> pPrint a
	pPrint ( If a ) = text "IF" <> pPrint a
	pPrint ( Elseif a ) = text "ELSEIF" <> pPrint a
	pPrint ( Else a ) = text "ELSE" <> pPrint a
	pPrint ( Case a ) = text "CASE: " <> pPrint a
	pPrint ( While a ) = text "WHILE" <> pPrint a
	pPrint ( Loop a ) = text "LOOP" <> pPrint a
	pPrint ( Repeat a ) = text "REPEAT" <> pPrint a
	pPrint ( Return a ) = text "RETURN" <> pPrint a		-- Aggiungere k - 1
	pPrint ( Break a ) = text "BREAK" <> pPrint a		-- Aggiungere k - 1
	pPrint ( Program p ) = vcat [ nest ( k + 1 ) ( pPrint p ) ]

{- con Procedure e Attribute che possono essere uno tra i seguenti:       ( A > attributes, P > procedures )
 "PROCEDURE"      P         
 "BEGIN"          P        
 "END"            P         
 "VAR"            A
 assegnamenti     A
 "IF"             P         
 "ELSIF"          P         
 "ELSE"           P         
 "CASE"           P         
 "WHILE"          P    
 "LOOP"		  P
 "REPEAT"         P       
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
