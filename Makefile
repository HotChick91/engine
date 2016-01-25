CC = gcc
CFLAGS = -lOpenCL -lglfw -lGL -lm -g -std=c11 -Wall

ALL: engine

engine: cl.o engine.o error.o geom.o globals.o render.o
	gcc -o $@ $^ $(CFLAGS)

clean:
	rm -f engine *.o

