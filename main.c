#include "ast.h"
#include <stdio.h>

int lineno = 1;
extern FILE* yyin;
void yyparse();

int main ( int argc, char* arg[] ) 
{
  if (argc>1)
     yyin = fopen(arg[1],"r");
  yyparse();
};