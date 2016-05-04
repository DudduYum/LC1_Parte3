-- queste funzioni vanno inserite nelle apposite regole della
-- gramatica

writeInt ::  Int-> IO ()
writeInt x = do
  putStr (show x)
  return ()

writeFloat ::  Float-> IO ()
writeFloat x = do
  putStr (show x)
  return ()

writeChar :: Char -> IO ()
writeChar x = do
  putStr (show x)
  return ()

writeString :: [Char] -> IO ()
writeString x= do
  putStr (show x)
  return ()


readChar :: IO Char
readChar = getChar

readInt ::  IO Int
readInt = do
  val <- getLine
  return (read val :: Int)

readFloat :: IO Float
readFloat = do
  val <- getLine
  return (read val :: Float)


readString :: IO String
readString = do
  val <- getLine
  return val
