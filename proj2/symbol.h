#ifndef _symbol_h_
#define _symbol_h_

typedef enum { STYPE_TERM, STYPE_NONTERM } SymbolType;

typedef enum {
    SYM_I,
    SYM_ADD,
    SYM_MINUS,
    SYM_MULTIPLY,
    SYM_DIVIDE,
    SYM_LPAREN,
    SYM_RPAREN,
    SYM_END
} TermSymbol;

typedef enum { SYM_E, SYM_G, SYM_T, SYM_S, SYM_F } NontermSymbol;

#define kTermSymbolMax SYM_END
#define kNontermSymbolMax SYM_F

typedef struct {
    SymbolType type;
    union {
        TermSymbol ts;
        NontermSymbol ns;
    };
    char repr[8];
} Symbol;

#endif  // _symbol_h_
