#include "string.h"
#include "stdio.h"
#include "stdlib.h"

/* Put the names of all the different kinds of ASTs here */

typedef enum 
{
    Program, BodyDef,
    Decs, VarDecs, TypeDecs, ProcDecs,
    VarDec, TypeDec, ProcDec,
    NamedTyp, ArrayTyp, RecordTyp, NoTyp,
    Comp, CompList,
    Param, FormalParamList,
    StList,
    AssignSt, CallSt, ReadSt, WriteSt, IfSt, WhileSt, LoopSt, ForSt, ExitSt, RetSt, SeqSt, EmptySt,
    ExprList,
    BinOpExp, UnOpExp, LvalExp, CallExp, ArrayExp, RecordExp,
    IntConst, RealConst, StringConst,
    Gt, Lt, Eq, Ge, Le, Ne, Plus, Minus, Times, Slash, Div, Mod, And, Or, UPlus, UMinus, Not,
    RecordInitList, RecordInit,
    ArrayInitList, ArrayInit,
    LvalList, Var,
    ArrayDeref, RecordDeref,
    TypeNameMissed
} ast_kind;

static char* ast_names[] = 
{
    "Program", "BodyDef",
    "Decs", "VarDecs", "TypeDecs", "ProcDecs",
    "VarDec", "TypeDec", "ProcDec",
    "NamedTyp", "ArrayTyp", "RecordTyp", "NoTyp",
    "Comp", "CompList",
    "Param", "FormalParamList",
    "StList",
    "AssignSt", "CallSt", "ReadSt", "WriteSt", "IfSt", "WhileSt", "LoopSt", "ForSt", "ExitSt", "RetSt", "SeqSt", "EmptySt",
    "ExprList",
    "BinOpExp", "UnOpExp", "LvalExp", "CallExp", "ArrayExp", "RecordExp",
    "IntConst", "RealConst", "StringConst",
    "Gt", "Lt", "Eq", "Ge", "Le", "Ne", "Plus", "Minus", "Times", "Slash", "Div", "Mod", "And", "Or", "UPlus", "UMinus", "Not",
    "RecordInitList", "RecordInit",
    "ArrayInitList", "ArrayInit",
    "LvalList", "Var",
    "ArrayDeref", "RecordDeref",
    "TypeNameMissed"
};

typedef struct ast 
{
  enum 
  { 
    int_ast, real_ast, var_ast, str_ast, node_ast 
  } tag;
  union 
  {
      int          integer;
      double        real;
      char*         variable;
      char*         string;
      struct 
      {
          ast_kind          tag;
          struct ast_list*  arguments;
	  } node;
  } info;
} ast;

typedef struct ast_list 
{ 
  ast*             elem;
  struct ast_list* next;
} ast_list;

/* create an integer AST leaf */
ast* mk_int ( const long x );


/* create a floating point AST leaf */
ast* mk_real ( const double x );


/* create an AST leaf for a name */
ast* mk_var ( const char* x );


/* create a string AST leaf */
ast* mk_str ( const char* x );


/* create an internal AST node */
ast* mk_node ( const ast_kind tag, ast_list* args );


/* put an AST e in the beginning of the list of ASTs r */
ast_list* cons ( ast* e, ast_list* r );

/* concatenate two lists of ASTs */
ast_list *concat( ast_list* a, ast_list* b );

/* the empty list of ASTs */
#define null NULL


/* size of an AST list */
short length ( ast_list* );

/* reverse the order of ASTs in an AST list */
ast_list* reverse ( ast_list* );


/* printing functions for ASTs */
void print_ast_list ( ast_list* r, int dep );

void print_ast ( ast* x, int dep );