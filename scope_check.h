#ifndef SCOPE_CHECK_H
#define SCOPE_CHECK_H

#include "ast.h"           // For accessing AST nodes
#include "symtab.h"   // For managing symbol table and scopes
#include "utilities.h"      // For error reporting

/* Function Prototypes */

/**
 * Initializes the symbol table and prepares for a new scope check.
 */
void scope_check_init(void);

/**
 * Checks the scope of declarations in the given program AST.
 * Traverses the AST and verifies that:
 *   - All identifiers used in expressions and statements are declared in the visible scope.
 *   - No identifier is redeclared in the same scope.
 * 
 * @param root - The root AST node of the SPL program.
 * @return true if the scope check passed without errors, false otherwise.
 */
bool scope_check_program(AST *root);

/**
 * Ends the scope check, freeing resources associated with the symbol table.
 */
void scope_check_finalize(void);

#endif // SCOPE_CHECK_H
