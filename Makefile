#  Makefile for simple calc
calc: calc.o parse.o parse.h
	cc -o calc calc.o parse.o -lm -lreadline
calc.o: calc.c parse.h
	cc -c -o calc.o calc.c
parse.o: parse.c parse.h
	cc -c -o parse.o parse.c
clean:
	rm -f parse.o calc calc.o
