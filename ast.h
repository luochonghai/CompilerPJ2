#include "string.h"
#include "stdio.h"
#include "stdlib.h"
#define null NULL

//Put the names of all the different kinds of ASTs here 
typedef enum 
{
    Program, BodyDef,Decs, VarDecs, TypeDecs, ProcDecs,VarDec, TypeDec, ProcDec,NamedTyp, 
    ArrayTyp, RecordTyp, NoTyp,Comp, CompList, Param, FormalParamList,StList,AssignSt, CallSt, 
    ReadSt, WriteSt, IfSt, WhileSt, LoopSt, ForSt, ExitSt, RetSt, SeqSt, EmptySt,
    ExprList,BinOpExp, UnOpExp, LvalExp, CallExp, ArrayExp, RecordExp,IntConst, RealConst, StringConst,
    Gt, Lt, Eq, Ge, Le, Ne, Plus, Minus, Times, Slash, Div, 
    Mod, And, Or, UPlus, UMinus, Not,RecordInitList, RecordInit,ArrayInitList, ArrayInit,
    LvalList, Var,ArrayDeref, RecordDeref,TypeNameMissed
} ast_kind;

static char* ast_class[] = 
{
    "program", "body_definition","declaration", "variable_declaration_array", "type_declaration_array", 
    "procedure_declaration_array","variable_declaration", "type_declaration", "procedure_declaration","named_type", 
    "array_type", "record_type", "no_type","component", "component_list","parametric", "formal_parametric_list",
    "statement_list","assign_statement", "call_statement", "read_statement", "write_statement", "if_statement", 
    "while_statement", "loop_statement", "for_statement", "exit_statement", "return_statement", "sequence_statement", 
    "empty_statement","expression_list","binary_operator_expression", "unary_operator_expression", "l_value_expession", 
    "call_expression", "array_expression", "record_expression","int_const", "real_const", "string_const",
    "greater_than", "less_than", "equal", "greater_equal", "less_equal", "not_equal", "plus", "minus", "times", "slash", "divide", 
    "module", "and", "or", "positive_sign", "negative_sign", "not","record_initialize_list", "record_initialize",
    "array_initialize_list", "array_initialize","l_value_list", "variable", "array_dereference", "record_dereference","type_name_missed"
};

typedef struct ast 
{
  enum {int_ast, real_ast, var_ast, str_ast, node_ast} tag;
  union {
      int integer;
      double real;
      char* variable;
      char* string;
      struct 
      {
          ast_kind tag;
          struct ast_list* arguments;
    } node;
  } info;
} ast;


typedef struct ast_list 
{ 
  ast* elem;
  struct ast_list* next;
} ast_list;

/*original 5 func */
// create an int ast leaf
ast* mk_int ( const long x );
//create a floating point ast leaf 
ast* mk_real ( const double x );
//create an ast leaf for a name
ast* mk_var ( const char* x );
//create a string ast leaf 
ast* mk_str ( const char* x );
//create an internal ast node 
ast* mk_node ( const ast_kind tag, ast_list* args );


//put ast e at the head of ast_list r 
ast_list* stick_list(ast* e, ast_list* r);
//join 2 ast_lists 
ast_list * join_list( ast_list* a, ast_list* b );
//reverse the order of ast_list
ast_list* rev_list( ast_list* );
//print ast in recursion
void p_ast_list ( ast_list* r, int dep );
void p_ast ( ast* x, int dep );