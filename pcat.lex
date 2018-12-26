%{

#include <stdio.h>
#include "y.tab.h"

void yyerror(char *s);
int yycheck_valid_char(char c);
extern int lineno;

%}
digit	[0-9]
alpha	[a-zA-Z]
whitespace	[ |\t|\r]+
digits	{digit}+

%x COMMENT

ENTER		\n
IDENTIFIER	{alpha}+({alpha}|{digit})*
INTEGER		{digits}
REAL		{digits}\.{digits}?|{digits}?\.{digits}
STRING		\"[^\n\"]*\"
UNTSTRING	\"[^\n\"]*\n

LPAREN		\(
RPAREN		\)
LBRACKET	\[
RBRACKET	\]
LBRACE		\{
RBRACE		\}
COLON		:
DOT		\.
SEMICOLON	;
COMMA		,
ASSIGN		:=
PLUS		\+
MINUS		-
STAR		\*
SLASH		\/
BACKSLASH	\\
EQ		=
NEQ		<>
LT		<
LE		<=
GT		>
GE		>=
LABRACKET	\[<
RABRACKET	>\]
%%


"(*"		{BEGIN(COMMENT);}
<COMMENT>"*)"	{BEGIN(INITIAL);}
<COMMENT>"\n" 	{lineno++;}
<COMMENT>.	{}
<COMMENT><<EOF>> {
	lineno--;
	yyerror("Unterminated comment");
	return EOFF;
}


{STRING} {
	if(yyleng-2 < 256) {
        	for(int i=1; i<yyleng-1; i++)
			if(yycheck_valid_char(yytext[i])) return ERROR;
		return STRINGT;
	}
	else {
		yyerror("String too long");
		return ERROR;
	}
}

{UNTSTRING} {
	yyerror("Unterminated string");lineno++;
	return ERROR;
}
PROGRAM     {return PROGRAM;}
IS          {return IS;}
BEGIN       {return BEGINT;}
END         {return END;}
VAR         {return VAR;}
TYPE        {return TYPE;}
PROCEDURE   {return PROCEDURE;}
ARRAY       {return ARRAY;}
RECORD      {return RECORD;}
IN          {return IN;}
OUT         {return OUT;}
READ        {return READ;}
WRITE       {return WRITE;}
IF          {return IF;}
THEN        {return THEN;}
ELSE        {return ELSE;}
ELSIF       {return ELSIF;}
WHILE       {return WHILE;}
DO          {return DO;}
LOOP        {return LOOP;}
FOR         {return FOR;}
EXIT        {return EXIT;}
RETURN      {return RETURN;}
TO          {return TO;}
BY          {return BY;}
AND         {return AND;}
OR          {return OR;}
NOT         {return NOT;}
OF          {return OF;}
DIV         {return DIV;}
MOD         {return MOD;}
{LPAREN}    {return LPAREN;}
{RPAREN}    {return RPAREN;}
{LBRACKET}  {return LBRACKET;}
{RBRACKET}  {return RBRACKET;}
{LBRACE}    {return LBRACE;}
{RBRACE}    {return RBRACE;}
{COLON}     {return COLON;}
{DOT}       {return DOT;}
{SEMICOLON} {return SEMICOLON;}
{COMMA}     {return COMMA;}
{ASSIGN}    {return ASSIGN;}
{PLUS}      {return PLUS;}
{MINUS}     {return MINUS;}
{STAR}      {return STAR;}
{SLASH}     {return SLASH;}
{BACKSLASH} {return BACKSLASH;}
{EQ}        {return EQ;}
{NEQ}       {return NEQ;}
{LT}        {return LT;}
{LE}        {return LE;}
{GT}        {return GT;}
{GE}        {return GE;}
{LABRACKET} {return LABRACKET;}
{RABRACKET} {return RABRACKET;}

{IDENTIFIER} {
	if(yyleng < 256)
		return IDENTIFIER;
	else {
		yyerror("Identifier too long");
		return ERROR;
	}
}

{INTEGER} {
	if(yyleng > 11) {
		yyerror("integer out of range");
		return ERROR;
	}
	else
		return INTEGERT;
}

{REAL}		{return REALT;}
{whitespace}	{}
{ENTER}		{lineno++;}
<<EOF>>		return 0;

%%






int yycheck_valid_char(char c)
{
	if(c >= 32 && c <= 126) return 0;
        printf("Error: \"Illegal character `\\%03o' ignored\" in line %d of stdin\n",
            c, lineno);
	return 1;
}

int yywrap()
{
	return 1;
}