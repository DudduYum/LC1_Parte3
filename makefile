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

.IGNORE: demo
demo:
	printf "Primo test che non da errori \n"
	./OLike < test/test1.txt
	printf "\n\nSecondo test che non da errori\n"
	./OLike < test/test2.txt
	printf "\n\nTerzo test che non da errori\n"
	./OLike < test/test3.txt
	printf "\n\nQuarto test che DA errori\n"
	./OLike < test/test4.txt
	printf "\n\nQuinto test che non da errori e comprende circa tutto\n"
	./OLike < test/test5.txt