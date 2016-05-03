CC=ghc

all: com

com: oberonTok.hs oberonGra.hs com.hs
	CC --make com 

oberonGra.hs : oberon.y
	happy -o obertonGra.hs oberon.y

oberonTok.hs : oberon.x
	alex -o obertonTok.hs oberon.x

clean:
	rm -f com obertonGra.hs obertonTok.hs *.o *.hi
