ALL: engine

engine:
	cabal configure --bindir=.
	cabal build
	cabal copy

clean:
	cabal clean
	rm -f engine
