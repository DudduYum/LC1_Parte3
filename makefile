CC=ghc

all: com

com: oberonLexer.hs oberonParser.hs oberonTools.hs
	ghc -Wall -o OLike oberonLexer.hs oberonParser.hs oberonTools.hs

oberonParser.hs : oberonParser.y
	happy -o oberonParser.hs oberonParser.y

oberonLexer.hs : oberonLexer.x
	alex -o oberonLexer.hs oberonLexer.x

clean:
	rm -f OLike oberonParser.hs oberonLexer.hs *.o *.hi
