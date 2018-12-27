# Makefile for the PCAT AST generation

GCC = gcc
CFLAGS = -g -std=gnu99
YACC = bison
LEX = flex

all: parser

parser: main.c y.tab.c lex.yy.c ast.h ast.o
	$(GCC) $(CFLAGS) main.c y.tab.c lex.yy.c ast.o -o parser
	rm -f lex.yy.c y.tab.* *.o

ast.o:  ast.c ast.h
	$(GCC) $(CFLAGS) -c ast.c

y.tab.c: syntax.y
	$(YACC) -d -y syntax.y

lex.yy.c: lexical.lex
	$(LEX) lexical.lex

clean:
	/bin/rm -f *.o *~ parser 