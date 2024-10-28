#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "id_attrs.h"  // For attributes of each identifier
#include <stdbool.h>   // For boolean type

// Define a struct for a single symbol (an entry in the table)
typedef struct Symbol {
    char *name;              // Name of the identifier
    IdAttrs attributes;       // Attributes from the id_attrs module
    struct Symbol *next;     // Pointer to the next symbol in case of a collision (for chaining)
} Symbol;

// Define a struct for a scope
typedef struct Scope {
    Symbol **symbols;         // Array of pointers to symbols (for hash table or list)
    struct Scope *parent;     // Pointer to the parent scope (for nested scopes)
    int num_symbols;          // Number of symbols in the scope
} Scope;

// Define a struct for the symbol table itself, which is a stack of scopes
typedef struct SymbolTable {
    Scope *current_scope;     // Pointer to the current scope (top of the scope stack)
} SymbolTable;

/* Function Prototypes */

// Initializes a new symbol table
SymbolTable *create_symbol_table(void);

// Adds a new scope to the symbol table (pushes onto scope stack)
void enter_scope(SymbolTable *table);

// Removes the current scope from the symbol table (pops from scope stack)
void exit_scope(SymbolTable *table);

// Adds a new symbol to the current scope
bool add_symbol(SymbolTable *table, const char *name, IdAttrs attributes);

// Finds a symbol in the current or any enclosing scope
Symbol *find_symbol(SymbolTable *table, const char *name);

// Deletes the entire symbol table and frees memory
void delete_symbol_table(SymbolTable *table);

#endif // SYMBOL_TABLE_H
