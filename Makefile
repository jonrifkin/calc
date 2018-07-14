calc: calc.c
	cc -o calc calc.c parse.c -lm -lreadline

clean: 
	@rm -f calc
