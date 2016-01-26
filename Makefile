all: engine

engine:
	stack install

clean:
	stack clean

.PHONY: clean all
