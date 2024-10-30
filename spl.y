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
%{
    #include <stdio.h>
    int yylex();
    void yyerror(const char *s);
%}

%union {
    int number;
    char *ident;
    char token; // For simple token types
    // Add structures for complex types if necessary
    struct block {
        // define members for block
    } block;

    struct const_decls {
        // define members for const declarations
    } const_decls;

    struct var_decls {
        // define members for var declarations
    } var_decls;

    struct var_decl {
        // define members for a single var declaration
    } var_decl;

    struct idents {
        // define members for identifiers
    } idents;

    struct proc_decls {
        // define members for procedure declarations
    } proc_decls;

    struct empty {
        // define members for empty structures
    } empty;

    struct const_decl {
        // define members for a single const declaration
    } const_decl;

    struct const_def {
        // define members for a const definition
    } const_def;

    struct const_defs {
        // define members for const definitions
    } const_defs;

    struct proc_decl {
        // define members for a procedure declaration
    } proc_decl;

    struct stmt {
        // define members for statements
    } stmt;

    struct assign_stmt {
        // define members for assignment statements
    } assign_stmt;

    struct call_stmt {
        // define members for call statements
    } call_stmt;

    struct begin_stmt {
        // define members for begin statements
    } begin_stmt;

    struct if_stmt {
        // define members for if statements
    } if_stmt;

    struct while_stmt {
        // define members for while statements
    } while_stmt;

    struct read_stmt {
        // define members for read statements
    } read_stmt;

    struct write_stmt {
        // define members for write statements
    } write_stmt;

    struct skip_stmt {
        // define members for skip statements
    } skip_stmt;

    struct stmts {
        // define members for multiple statements
    } stmts;

    struct condition {
        // define members for conditions
    } condition;

    struct odd_condition {
        // define members for odd conditions
    } odd_condition;

    struct rel_op_condition {
        // define members for relational operation conditions
    } rel_op_condition;

    struct expr {
        // define members for expressions
    } expr;
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

// Grammar Rules
program:
    block_stmt semisym   // A program consists of a block statement followed by a semicolon
    ;

block_stmt:
    lparensym statements rparensym  // A block statement enclosed in parentheses
    ;

statements:
    statement  // A single statement
    | statements statement  // Multiple statements
    ;

statement:
    printStmt  // A print statement
    | assignmentStmt  // An assignment statement
    ;

printStmt:
    plussym lparensym expression rparensym  // Print statement syntax
    ;

assignmentStmt:
    identsym becomessym expression  // Assignment statement syntax
    ;

expression:
    term  // An expression can be a term
    | expression plussym term  // Or an expression followed by a plus and another term
    | expression minussym term  // Or an expression followed by a minus and another term
    ;

term:
    factor  // A term can be a factor
    | term multsym factor  // Or a term followed by a multiplication and another factor
    | term divsym factor  // Or a term followed by a division and another factor
    ;

factor:
    numbersym  // A factor can be a number
    | identsym  // Or an identifier
    | lparensym expression rparensym  // Or an expression enclosed in parentheses
    ;




%%

// Set the program's ast to be ast
void setProgAST(block_t ast) { progast = ast; }

