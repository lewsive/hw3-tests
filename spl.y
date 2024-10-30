 /* $Id: bison_spl_y_top.y,v 1.2 2024/10/09 18:18:55 leavens Exp $ */

%code top {
#include <stdio.h>
}

%code requires {

 /* Including "ast.h" must be at the top, to define the AST type */
#include "ast.h"
#include "machine_types.h"
#include "parser_types.h"
#include "lexer.h"

    /* Report an error to the user on stderr */
extern void yyerror(const char *filename, const char *msg);

}    /* end of %code requires */
%union {
    int number;               // For integer values
    char *ident;              // For identifiers
    char token;               // For single-character tokens
    block_stmt_t block;       // For block statements
    stmts_t statements;       // For lists of statements
    stmt_t statement;         // For a single statement
    expr_t expression;        // For expressions
    term_t term;             // For terms
    factor_t factor;          // For factors
    condition_t condition;     // For conditions (if, while, etc.)
    db_condition_t dbCondition; // For database conditions
    rel_op_t relOp;           // For relational operators
    const_decls_t constDecls; // For constant declarations
    var_decls_t varDecls;     // For variable declarations
    proc_decls_t procDecls;   // For procedure declarations
}

%verbose
%define parse.lac full
%define parse.error detailed

 /* the following passes file_name to yyerror,
    and declares it as an formal parameter of yyparse. */
%parse-param { char const *file_name }

%token <ident> identsym
%token <number> numbersym
%token <token> plussym    "+"
%token <token> minussym   "-"
%token <token> multsym    "*"
%token <token> divsym     "/"
%token <token> periodsym  "."
%token <token> semisym    ";"
%token <token> eqsym      "="
%token <token> commasym   ","
%token <token> becomessym ":="
%token <token> lparensym  "("
%token <token> rparensym  ")"

%token <token> constsym   "const"
%token <token> varsym     "var"
%token <token> procsym    "proc"
%token <token> callsym    "call"
%token <token> beginsym   "begin"
%token <token> endsym     "end"
%token <token> ifsym      "if"
%token <token> thensym    "then"
%token <token> elsesym    "else"
%token <token> whilesym   "while"
%token <token> dosym      "do"
%token <token> readsym    "read"
%token <token> printsym   "print"
%token <token> divisiblesym "divisible"
%token <token> bysym      "by"

%token <token> eqeqsym    "=="
%token <token> neqsym     "!="
%token <token> ltsym      "<"
%token <token> leqsym     "<="
%token <token> gtsym      ">"
%token <token> geqsym     ">="

%type <block> program

%type <block> block

%type <const_decls> constDecls
%type <const_decl> constDecl
%type <const_def_list> constDefList
%type <const_def> constDef

%type <var_decls> varDecls
%type <var_decl> varDecl
%type <ident_list> identList

%type <proc_decls> procDecls
%type <proc_decl> procDecl


%type <stmts> stmts
%type <empty> empty
%type <stmt_list> stmtList
%type <stmt> stmt
%type <assign_stmt> assignStmt
%type <call_stmt> callStmt
%type <if_stmt> ifStmt
%type <while_stmt> whileStmt
%type <read_stmt> readStmt
%type <print_stmt> printStmt
%type <block_stmt> blockStmt

%type <condition> condition
%type <db_condition> dbCondition
%type <rel_op_condition> relOpCondition
%type <token> relOp

%type <expr> expr
%type <expr> term
%type <expr> factor

%start program

%code {
 /* extern declarations provided by the lexer */
extern int yylex(void);

 /* The AST for the program, set by the semantic action 
    for the nonterminal program. */
block_t progast; 

 /* Set the program's ast to be t */
extern void setProgAST(block_t t);
}

%%
 /* Write your grammar rules below and before the next %% */

program:
    block { $$ = setProgAST($1); }
    ;

block:
    beginsym constDecls varDecls procDecls stmts endsym
    { $$ = ast_block($1, $2, $3); }
    ;

const_decls:
    /*handle empty const_desls */
    { $$ = ast_const_decls_empty(empty); }
    | const_decls const_decl
    { $$ = ast_const_decls($1, $2); }
    ;

const_decl:
    const const_def_list
    { $$ = ast_const_decl($1); }
    ;

const_def_list:
    const_def
    { $$ = ast_const_def_list_singleton($1); }
    | const_def_list commasym const_def
    { $$ = ast_const_def_list($1, $2); }
    ;

const_def:
    ident eqsym number
    { ast_const_def($1, $2); }
    ;

var_decls:
    /*handle empty var_decls */
    { $$ = ast_var_decls_empty(empty); }
    | var_decls var_decl
    { $$ = ast_var_decls($1, $2); }
    ;

var_decl:
    var ident_list
    { $$ = ast_var_decl($1); }
    ;

ident_list:
    ident
    { ast_ident_list_singleton($1); }
    | ident_list commasym ident
    { $$ = ast_const_def_list($1, $2); }
    ;

proc_decls:
    /*handle empty proc_decls */
    { $$ = ast_proc_decls_empty(empty); }
    | proc_decls proc_decl
    { $$ = ast_proc_decls($1, $2); }
    ;

proc_decl:
    proc ident block
    { ast_proc_decl($1, $2); }
    ;

stmts:
    /*handle empty proc_decls */
    { $$ = ast_stmts_empty(empty); }
    | stmt_list
    { $$ = ast_stmts($1); }
    ;

empty:
    empty
    { $$ = ast_empty($1); }
    ;

stmt_list:
    stmt
    { $$ = ast_stmt_list_singleton($1); }
    | stmt_list semisym stmt
    { $$ = ast_stmt_list($1, $2); }
    ;

stmt:
    assign_stmt
    { $$ = ast_stmt_assign($1); }
    | call_stmt
    { $$ = ast_stmt_call($1); }
    | if_stmt
    { $$ = ast_stmt_if($1); }
    | while_stmt
    { $$ = ast_stmt_while($1); }
    | read_stmt
    { $$ = ast_stmt_read($1); }
    | print_stmt
    { $$ = ast_stmt_print($1); }
    | block_stmt
    { $$ = ast_stmt_block($1); }
    ;

assign_stmt:
    ident becomessym expr
    { $$ = ast_assign_stmt($1, $2); }
    ;

call_stmt:
    call ident
    { $$ = ast_call_stmt($1); }
    ;

ifStmt:
    "if" condition "then" stmts "else" stmts "end"
    { $$ = ast_if_then_else_stmt($1, $2, $3); }
    | "if" condition "then" stmts "end"
    { $$ = ast_if_then_stmt($1, $2); }
    ;

while_stmt:
    while condition do stmts end
    { $$ = ast_while_stmt($1, $2); }
    ;

read_stmt:
    read ident
    { $$ = ast_read_stmt($1); }
    ;

print_stmt:
    print expr
    { $$ = ast_print_stmt($1); }
    ;

%%


// Set the program's ast to be ast
void setProgAST(block_t ast) { progast = ast; }

