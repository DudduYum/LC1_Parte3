 
{-
 
[Declaration 
    {declarationType = DT_Procedure, attributeDeclared = Nothing, 
                procedureDeclared = Just 
                        (Procedure {procedureName = "p1", attributes = [],
                        procedureProcedures = [Procedure 
                            {procedureName = "p12", attributes = [], procedureProcedures = [Procedure           {procedureName = "p123", attributes = [], procedureProcedures = []
                            }]},
                        Procedure {procedureName = "p22", attributes = [], procedureProcedures = [Procedure {procedureName = "p1234", attributes = [], procedureProcedures = []}]}]})}]

                        
                        
                        
                        
result: 
[Declaration {declarationType = DT_Procedure, attributeDeclared = Nothing, procedureDeclared = Just       
    (Procedure {procedureName = "p1", attributes = [], 
        procedureProcedures = [Procedure {procedureName = "p12", attributes = [], 
            procedureProcedures = [Procedure {procedureName = "p123", attributes = [], 
                procedureProcedures = []}
            ]},
        Procedure {procedureName = "p22", attributes = [], 
            procedureProcedures = [Procedure {procedureName = "p1234", attributes = [], 
                procedureProcedures = []}
            ]
        }
    ]})
}]
DONE



Regole per indentazione:

- se si incontra una parentesi tonda aperta '(', si va a capo;
- se si incontra la parola 'procedureProcedures' si va a capo;
- se si incontra una parentesi tonda chiusa ')', si va a capo;
- se si incontra una parentesi graffa chiusa '}' si va a capo;
- tenere conto delle indentazioni precedenti; ( variabile intera, che si aumenta o decrementa, per numero indentazioni )


main = do
    c <- getChar
    if c == '(' or ')' or '}' then
        return '\n' ++ '\t' ++ c
    else do
        xs <- getLine
        return c:xs
        
        
        
        
        

x <- getLine 
y <- spliton " " x       ( [x] )
printer y:
do
    first <- head y
    if head first == '(' or ')' or '}' then
        return '\n' ++ '\t' ++ first                    ( o print )
    else if first == "procedureProcedures" then
        return '\n' ++ '\t' ++ first
    else
        printer tail y

-}


        
--Versione quasi definitiva
        
        

   
import Data.List.Split
--import qualified Data.Text as T
   
        
--splitOn :: Char -> [Char] -> [[Char]]

--splitOn :: T.Text -> T.Text -> [T.Text]

printer :: [[Char]] -> IO ()                        -- Non restituisce valori

printer xs = do
    let first = head xs
    if head first == '(' then
        print '\n' ++ '\t' ++ first ++ printer tail xs
    else if head first == ')' || '}' then
        print '\n' ++ first ++ printer tail xs
    else if first == "procedureProcedures" then
        print '\n' ++ '\t' ++ first ++ printer tail xs
    else
        printer tail xs


main = do
    x <- getLine
    let y = words x
    return printer y












