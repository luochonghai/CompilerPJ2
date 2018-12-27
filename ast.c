#include "ast.h"
#define Ind 4

ast* mk_int ( const long x ) 
{
  ast* res = (ast*) malloc(sizeof(ast));
  res->tag = int_ast;
  res->info.integer = x;
  return res;
};

ast* mk_real ( const double x ) 
{
  ast* res = (ast*) malloc(sizeof(ast));
  res->tag = real_ast;
  res->info.real = x;
  return res;
};

ast* mk_var ( const char* x ) 
{
  ast* res = (ast*) malloc(sizeof(ast));
  res->tag = var_ast;
  res->info.variable = (char*) malloc(strlen(x)+1);
  strcpy(res->info.variable,x);
  return res;
};

ast* mk_str ( const char* x ) 
{
  ast* res = (ast*) malloc(sizeof(ast));
  res->tag = str_ast;
  res->info.variable = (char*) malloc(strlen(x)+1);
  strcpy(res->info.variable,x);
  return res;
};

ast* mk_node ( const ast_kind tag, ast_list* args ) 
{
  ast* res = (ast*) malloc(sizeof(ast));
  res->tag = node_ast;
  res->info.node.tag = tag;
  res->info.node.arguments = args;
  return res;
};

ast_list* stick_list ( ast* e, ast_list* r ) 
{
  ast_list* res = (ast_list*) malloc(sizeof(ast_list));
  res->elem = e;
  res->next = r;
  return res;
};

ast_list* join_list ( ast_list* a, ast_list* b ) 
{
    ast_list* res = NULL;
    while (a != NULL && a->elem != NULL) {
        res = stick_list(a->elem, res);
        a = a->next;
    }
    while (b != NULL && b->elem != NULL) {
        res = stick_list(b->elem, res);
        b = b->next;
    }
    res = rev_list(res);
    return res;
}

ast_list* rev ( ast_list* r, ast_list* s ) 
{
  if (r == null)
     return s;
  return rev(r->next,stick_list(r->elem,s));
};


ast_list* rev_list( ast_list* r ) 
{
  return rev(r,null);
};


void p_ast_list ( ast_list* r, int dep ) 
{
  if (r == null)
     return;
  p_ast(r->elem, dep + 1);
  p_ast_list(r->next, dep);
};


void p_ast ( ast* x, int dep ) 
{
    int i;
    for (i = 0; i < dep * Ind; ++i)
        printf(" ");
    switch (x->tag) 
    {
        case int_ast: printf("INTEGER (%d)\n",x->info.integer); break;
        case real_ast: printf("REAL (%f)\n",x->info.real); break;
        case var_ast: printf("%s\n",x->info.variable); break;
        case str_ast: printf("STRING (%s)\n",x->info.string); break;
        case node_ast: 
        {
            printf("%s\n", ast_class[x->info.node.tag]);
            p_ast_list(x->info.node.arguments, dep);
            break;
        }
    }
}