%{
#include "y.tab.h"
#include "ast.h"

extern int lineno;
extern char* yytext;

/* parse error */
void yyerror ( char* s ) {
    printf("ERROR: %s (line : %d, token: %s)\n", s, lineno, yytext);
};
%}

%union 
{
        struct ast*            Node;
        struct ast_list*       List;
}

%token IDENTIFIER INTEGERT REALT STRINGT
       PROGRAM IS BEGINT END VAR TYPE PROCEDURE ARRAY RECORD
       IN OUT READ WRITE IF THEN ELSE ELSIF WHILE DO LOOP
       FOR EXIT RETURN TO BY AND OR NOT OF DIV MOD
       LPAREN  RPAREN LBRACKET RBRACKET LBRACE RBRACE COLON DOT
       SEMICOLON COMMA ASSIGN PLUS MINUS STAR SLASH BACKSLASH EQ
       NEQ LT LE GT GE LABRACKET RABRACKET EOFF ERROR

%type <Node> program
             body
             declarations
             statements
             declaration
             var_dec_type
             type_dec
             proc_dec
             proc_dec_type
             typename
             type
             component
             formal_params
             statement
             statement_else
             statement_by
             write_expr
             expression_aux
             expression
             lvalue
             actual_params
             record_inits
             array_inits
             array_init
             number
             identifier
             string
%type <List> write_params_exprs
             statement_lvalues
             statement_elsifs
             write_params
             fp_section
             fp_section_ids
             fp_sections
             components
             record_inits_pairs
             statements_aux
             actual_params_exprs
             array_inits_array_inits
             declarations_aux
             var_decs
             type_decs
             proc_decs
             var_dec
             var_dec_ids


%nonassoc    ASSIGN
%left        OR
%left        AND
%nonassoc    EQ NEQ
%nonassoc    LT LE GT GE
%left        PLUS MINUS
%left        STAR DIV MOD
%nonassoc    VUMinus
%nonassoc    LBRACKET DOT

%%

start:              program                                 { print_ast($1, 0); }
;

program:            PROGRAM IS body SEMICOLON               { $$ = mk_node(Program, cons($3, NULL)); }
;

body:               declarations BEGINT statements END      { $$ = mk_node(BodyDef, cons($1, cons($3, NULL))); }
;

declarations:       declarations_aux                        { $$ = mk_node(Decs, reverse($1)); }
;

declarations_aux:   declarations_aux declaration            { $$ = cons($2, $1); }
                    |                                       { $$ = NULL; }
;

statements:         statements_aux                          { $$ = mk_node(SeqSt, reverse($1)); }
;

statements_aux:     statements_aux statement                { $$ = cons($2, $1); }
                    |                                       { $$ = NULL; }
;

declaration:        VAR var_decs                            { $$ = mk_node(VarDecs, $2); }
                    | TYPE type_decs                        { $$ = mk_node(TypeDecs, reverse($2)); }
                    | PROCEDURE proc_decs                   { $$ = mk_node(ProcDecs, reverse($2)); }
;

var_decs:           var_decs var_dec                        { $$ = concat($1, $2); }
                    |                                       { $$ = NULL; }
;

type_decs:          type_decs type_dec                      { $$ = cons($2, $1); }
                    |                                       { $$ = NULL; }
;

proc_decs:          proc_decs proc_dec                      { $$ = cons($2, $1); }
                    |                                       { $$ = NULL; }
;

var_dec:            identifier var_dec_ids var_dec_type ASSIGN expression SEMICOLON
                    {
                        struct ast_list *ids = cons($1, reverse($2));
                        $$ = NULL;
                        struct ast_list *vars = NULL;
                        while (ids != NULL) {
                            vars = cons(mk_node(VarDec, cons(ids->elem, cons($3, cons($5, NULL)))), vars);
                            ids = ids->next;
                        }
                        $$ = reverse(vars);
                    }
;

var_dec_ids:        var_dec_ids COMMA identifier            { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

var_dec_type:       COLON typename                          { $$ = $2; }
                    |                                       { $$ = mk_node(TypeNameMissed, NULL); }
;

type_dec:           identifier IS type SEMICOLON            { $$ = mk_node(TypeDec, cons($1, cons($3, NULL))); }
;

proc_dec:           identifier formal_params proc_dec_type IS body SEMICOLON
                    {
                        $$ = mk_node(ProcDec, cons($1, cons($2, cons($3, cons($5, NULL)))));
                    }
;

proc_dec_type:      COLON typename                          { $$ = $2; }
                    |                                       { $$ = mk_node(NoTyp, NULL); }
;

typename:           identifier                              { $$ = mk_node(NamedTyp, cons($1, NULL)); }
;

type:               ARRAY OF typename                       { $$ = mk_node(ArrayTyp, cons($3, NULL)); }
                    | RECORD component components END       { $$ = mk_node(RecordTyp, cons(mk_node(CompList, cons($2, reverse($3))), NULL)); }
;

components:         components component                    { $$ = cons($2, $1); }
                    |                                       { $$ = NULL; }
;

component:          identifier COLON typename SEMICOLON     { $$ = mk_node(Comp, cons($1, cons($3, NULL))); }
;

formal_params:      LPAREN fp_section fp_sections RPAREN    { $$ = mk_node(FormalParamList, concat($2, $3)); }
                    | LPAREN RPAREN                         { $$ = mk_node(FormalParamList, NULL); }
;

fp_sections:        fp_sections SEMICOLON fp_section        { $$ = concat($1, $3); }
                    |                                       { $$ = NULL; }
;

fp_section:         identifier fp_section_ids COLON typename
                    {
                        struct ast_list *ids = cons($1, reverse($2));
                        $$ = NULL;
                        struct ast_list *params = NULL;
                        while (ids != NULL) {
                            params = cons(mk_node(Param, cons(ids->elem, cons($4, NULL))), params);
                            ids = ids->next;
                        }
                        $$ = reverse(params);
                    }
;

fp_section_ids:     fp_section_ids COMMA identifier         { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

statement:          lvalue ASSIGN expression SEMICOLON      { $$ = mk_node(AssignSt, cons($1, cons($3, NULL))); }
                    | identifier actual_params SEMICOLON    { $$ = mk_node(CallSt, cons($1, cons($2, NULL))); }
                    | READ LPAREN lvalue statement_lvalues RPAREN SEMICOLON
                                                            { $$ = mk_node(ReadSt, cons($3, reverse($4))); }
                    | WRITE write_params SEMICOLON          { $$ = mk_node(WriteSt, $2); }
                    | IF expression THEN statements statement_elsifs statement_else END SEMICOLON
                    {
                        struct ast* if_ast = mk_node(IfSt, cons($2, cons($4, cons(NULL,NULL))));
                        struct ast* current_if = if_ast;
                        struct ast_list* middle_list = reverse($5);
                        while (middle_list != NULL) {
                            current_if->info.node.arguments->next->next = cons(middle_list->elem, NULL);
                            current_if = current_if->info.node.arguments->next->next->elem;
                            middle_list = middle_list->next;
                        }
                        current_if->info.node.arguments->next->next = cons($6, NULL);
                        $$ = if_ast;
                    }
                    | WHILE expression DO statements END SEMICOLON
                                                            { $$ = mk_node(WhileSt, cons($2, cons($4, NULL))); }
                    | LOOP statements END SEMICOLON         { $$ = mk_node(LoopSt, cons($2, NULL)); }
                    | FOR identifier ASSIGN expression TO expression statement_by DO statements END SEMICOLON
                    {
                        $$ = mk_node(ForSt, cons($2, cons($4, cons($6, cons($7, cons($9, NULL))))));
                    }
                    | EXIT SEMICOLON                        { $$ = mk_node(ExitSt, NULL); }
                    | RETURN expression_aux SEMICOLON       { $$ = mk_node(RetSt, cons($2, NULL)); }
;

statement_lvalues:  statement_lvalues COMMA lvalue          { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

statement_elsifs:   statement_elsifs ELSIF expression THEN statements
                                                            { $$ = cons(mk_node(IfSt, cons($3, cons($5, cons(NULL, NULL)))), $1); }
                    |                                       { $$ = NULL; }
;

statement_else:     ELSE statements                         { $$ = $2; }
                    |                                       { $$ = mk_node(EmptySt, NULL); }
;

statement_by:       BY expression                           { $$ = $2; }
                    |                                       { $$ = mk_int(1); }
;

write_params:       LPAREN write_expr write_params_exprs RPAREN
                                                            { $$ = cons($2, reverse($3)); }
                    | LPAREN RPAREN                         { $$ = NULL; }
;

write_params_exprs: write_params_exprs COMMA write_expr     { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

write_expr:         string                                  { $$ = $1; }
                    | expression                            { $$ = $1; }
;

expression_aux:     expression                              { $$ = $1; }
                    |                                       { $$ = mk_node(EmptySt, NULL); }
;

expression:         number                                  { $$ = $1; }
                    | lvalue                                { $$ = mk_node(LvalExp, cons($1, NULL)); }
                    | LPAREN expression RPAREN              { $$ = $2; }
                    | PLUS expression                       { $$ = mk_node(UnOpExp, cons(mk_node(UPlus, NULL), cons($2, NULL))); }
                    | MINUS expression %prec VUMinus        { $$ = mk_node(UnOpExp, cons(mk_node(UMinus, NULL), cons($2, NULL))); }
                    | NOT expression                        { $$ = mk_node(UnOpExp, cons(mk_node(Not, NULL), cons($2, NULL))); }
                    | expression PLUS expression            { $$ = mk_node(BinOpExp, cons(mk_node(Plus, NULL), cons($1, cons($3, NULL)))); }
                    | expression MINUS expression           { $$ = mk_node(BinOpExp, cons(mk_node(Minus, NULL), cons($1, cons($3, NULL)))); }
                    | expression STAR expression            { $$ = mk_node(BinOpExp, cons(mk_node(Times, NULL), cons($1, cons($3, NULL)))); }
                    | expression SLASH expression           { $$ = mk_node(BinOpExp, cons(mk_node(Slash, NULL), cons($1, cons($3, NULL)))); }
                    | expression DIV expression             { $$ = mk_node(BinOpExp, cons(mk_node(Div, NULL), cons($1, cons($3, NULL)))); }
                    | expression MOD expression             { $$ = mk_node(BinOpExp, cons(mk_node(Mod, NULL), cons($1, cons($3, NULL)))); }
                    | expression OR expression              { $$ = mk_node(BinOpExp, cons(mk_node(Or, NULL), cons($1, cons($3, NULL)))); }
                    | expression AND expression             { $$ = mk_node(BinOpExp, cons(mk_node(And, NULL), cons($1, cons($3, NULL)))); }
                    | expression EQ expression              { $$ = mk_node(BinOpExp, cons(mk_node(Eq, NULL), cons($1, cons($3, NULL)))); }
                    | expression NEQ expression             { $$ = mk_node(BinOpExp, cons(mk_node(Ne, NULL), cons($1, cons($3, NULL)))); }
                    | expression LT expression              { $$ = mk_node(BinOpExp, cons(mk_node(Lt, NULL), cons($1, cons($3, NULL)))); }
                    | expression LE expression              { $$ = mk_node(BinOpExp, cons(mk_node(Le, NULL), cons($1, cons($3, NULL)))); }
                    | expression GT expression              { $$ = mk_node(BinOpExp, cons(mk_node(Gt, NULL), cons($1, cons($3, NULL)))); }
                    | expression GE expression              { $$ = mk_node(BinOpExp, cons(mk_node(Ge, NULL), cons($1, cons($3, NULL)))); }
                    | identifier actual_params              { $$ = mk_node(CallExp, cons($1, cons($2, NULL))); }
                    | identifier record_inits               { $$ = mk_node(RecordExp, cons($1, cons($2, NULL))); }
                    | identifier array_inits                { $$ = mk_node(ArrayExp, cons($1, cons($2, NULL))); }
;

lvalue:             identifier                              { $$ = mk_node(Var, cons($1, NULL)); }
                    | lvalue LBRACKET expression RBRACKET   { $$ = mk_node(ArrayDeref, cons($1, cons($3, NULL))); }
                    | lvalue DOT identifier                 { $$ = mk_node(RecordDeref, cons($1, cons($3, NULL))); }
;

actual_params:      LPAREN expression actual_params_exprs RPAREN
                                                            { $$ = mk_node(ExprList, cons($2, reverse($3))); }
                    | LPAREN RPAREN                         { $$ = mk_node(ExprList, NULL); }
;

actual_params_exprs: actual_params_exprs COMMA expression   { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

record_inits:       LBRACE identifier ASSIGN expression record_inits_pairs RBRACE
                                                            { $$ = mk_node(RecordInitList, cons(mk_node(RecordInit, cons($2, cons($4, NULL))), reverse($5))); }
;
record_inits_pairs: record_inits_pairs SEMICOLON identifier ASSIGN expression
                                                            { $$ = cons(mk_node(RecordInit, cons($3, cons($5, NULL))), $1); }
                    |                                       { $$ = NULL; }
;

array_inits:        LABRACKET array_init array_inits_array_inits RABRACKET
                                                            { $$ = mk_node(ArrayInitList, cons($2, reverse($3))); }
;

array_inits_array_inits: array_inits_array_inits COMMA array_init
                                                            { $$ = cons($3, $1); }
                    |                                       { $$ = NULL; }
;

array_init:         expression                              { $$ = mk_node(ArrayInit, cons($1, NULL)); }
                    | expression OF expression              { $$ = mk_node(ArrayInit, cons($1, cons($3, NULL))); }
;

number:             INTEGERT                                { $$ = mk_int(atoi(yytext)); }
                    | REALT                                 { $$ = mk_real(atof(yytext)); }
;

identifier:         IDENTIFIER                              { $$ = mk_var(yytext); }
;

string:             STRINGT                                 { $$ = mk_str(yytext); }
;

%%