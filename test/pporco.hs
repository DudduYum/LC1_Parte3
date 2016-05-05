
fun :: IO ()
fun = do
  x <- getChar
  y <- getChar
  return (x++y)


main :: IO Char
main = do
  x <- getChar

  return x
