CC=ghc

all: com

com: oberonLexer.hs oberonParser.hs oberonTools.hs
	CC --make com 

oberonParser.hs : oberonParser.y
	happy -o oberonParser.hs oberonParser.y

oberonLexer.hs : oberonLexer.x
	alex -o oberonLexer.hs oberonLexer.x

clean:
	rm -f com oberonParser.hs oberonLexer.hs *.o *.hi
