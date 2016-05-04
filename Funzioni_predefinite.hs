
{-
// Forse le procedure di write utilizzano al loro interno le funzioni di read
// Da verificare...

Procedure:

writeInt

writeReal

writeChar

writeString


Funzioni:

readInt

readReal

readChar

readString


-------------------------------------


Serve utilizzare input e output

-}
--import Char



readInt :: Char -> Int

x <- getLine

let y = read x :: Int

return y



readReal :: Char -> Float

x <- getLine

let y = read x :: Float

return y



readChar :: Char -> Char

x <- getLine

return x



readString :: String -> String

x <- getLine

return x



writeInt :: Int -> Int

main = do
	x <- readInt
	print x



writeChar :: Char -> Char

main = do
	x <- readChar
	print x



writeReal :: Float -> Float

main = do
	x <- readFloat
	print x



writeString :: String -> String

main = do
	x <- readString
	print x


---------------------------------

{-writeInt
writeInt :: Int -> Int
main = do
    x <- readInt
    show x


--writeChar
writeChar :: Char -> Char
main = do
    x <- readChar
    show x


--writeReal
writeReal :: Float -> Float
main = do
    x <- readReal
    show x


--writeString
writeString :: String -> String
main = do
    x <- readString
    show x
-}
