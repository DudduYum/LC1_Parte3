CC=ghc


exe: oberon.hs
	CC -o exe oberon.hs

oberon.hs: oberon.x
	alex oberon.x
