CC = gcc
CFLAGS = -lglfw -lGL -lm -g -std=c11 -Wall -I /usr/lib/ghc-7.10.3/include/
LDFLAGS = -lglfw -lGL -lm .cabal-sandbox/lib/i386-linux-ghc-7.10.3/*/*.so /usr/lib/ghc-7.10.3/bytes_6VWy06pWzJq9evDvK2d4w6/libHSbytestring-0.10.6.0-6VWy06pWzJq9evDvK2d4w6-ghc7.10.3.so

ALL: engine

engine: engine.o FileLoader.o
	ghc -no-hs-main -o $@ $^ ${LDFLAGS}

FileLoader.o: FileLoader.hs
	cabal build
	cp dist/build/FileLoader.o .
	cp dist/build/FileLoader_stub.h .
	#ghc --make -c FileLoader.hs

clean:
	rm -f engine *.o FileLoader.h FileLoader.hi FileLoader_stub.h

