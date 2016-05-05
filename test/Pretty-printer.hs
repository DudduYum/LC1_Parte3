 
 
-- Qualche prova per capirsi, sul pretty printer
 
module Pretty-printer where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- Qui bisognerà implementare la struttura dati utilizzata dal parser

data Program a =        Procedure ( Program a )
               |        Attribute a deriving (Show)
               
               
-- Si procede poi a definire come sarà l'indentazione per casi
               
instance Pretty a => Pretty ( Program a ) where
    pPrint ( Attribute a ) = text "VAR: " <> pPrint a
    pPrint ( Procedure p ) = vcat [ text "PROCEDURE: "
                                    , nest 2 ( pPrint p )]
                                    

               
