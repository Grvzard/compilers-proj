// this header can only be included in a '.c' file once and only once
#ifndef _parsing_table_h_
#define _parsing_table_h_

#include "symbol.h"

const Symbol kSymbolE = {STYPE_NONTERM, .ns = SYM_E, "E"};
const Symbol kSymbolG = {STYPE_NONTERM, .ns = SYM_G, "G"};
const Symbol kSymbolT = {STYPE_NONTERM, .ns = SYM_T, "T"};
const Symbol kSymbolS = {STYPE_NONTERM, .ns = SYM_S, "S"};
const Symbol kSymbolF = {STYPE_NONTERM, .ns = SYM_F, "F"};

const Symbol kSymbolI = {STYPE_TERM, .ts = SYM_I, "i"};
const Symbol kSymbolAdd = {STYPE_TERM, .ts = SYM_ADD, "+"};
const Symbol kSymbolMinus = {STYPE_TERM, .ts = SYM_MINUS, "-"};
const Symbol kSymbolMultiply = {STYPE_TERM, .ts = SYM_MULTIPLY, "*"};
const Symbol kSymbolDivide = {STYPE_TERM, .ts = SYM_DIVIDE, "/"};
const Symbol kSymbolLeftParen = {STYPE_TERM, .ts = SYM_LPAREN, "("};
const Symbol kSymbolRightParen = {STYPE_TERM, .ts = SYM_RPAREN, ")"};
const Symbol kSymbolEnd = {STYPE_TERM, .ts = SYM_END, "#"};

typedef struct {
    const char repr[16];
    const Symbol* rhs[8];
    int rhs_len;
} ParsingRule;

const ParsingRule kParsingTable[kNontermSymbolMax + 1][kTermSymbolMax + 1] = {
    [SYM_E][SYM_I] = {"E->TG", {&kSymbolT, &kSymbolG}, 2},
    [SYM_E][SYM_ADD] = {"", {}},
    [SYM_E][SYM_MINUS] = {"", {}},
    [SYM_E][SYM_MULTIPLY] = {"", {}},
    [SYM_E][SYM_DIVIDE] = {"", {}},
    [SYM_E][SYM_LPAREN] = {"E->TG", {&kSymbolT, &kSymbolG}, 2},
    [SYM_E][SYM_RPAREN] = {"", {}},
    [SYM_E][SYM_END] = {"", {}},

    [SYM_G][SYM_I] = {"G->-TG", {&kSymbolMinus, &kSymbolT, &kSymbolG}, 3},
    [SYM_G][SYM_ADD] = {"G->+TG", {&kSymbolAdd, &kSymbolT, &kSymbolG}, 3},
    [SYM_G][SYM_MINUS] = {"G->-TG", {&kSymbolMinus, &kSymbolT, &kSymbolG}, 3},
    [SYM_G][SYM_MULTIPLY] = {"", {}},
    [SYM_G][SYM_DIVIDE] = {"", {}},
    [SYM_G][SYM_LPAREN] = {"", {}},
    [SYM_G][SYM_RPAREN] = {"G->ε", {}},
    [SYM_G][SYM_END] = {"G->ε", {}},

    [SYM_T][SYM_I] = {"T->FS", {&kSymbolF, &kSymbolS}, 2},
    [SYM_T][SYM_ADD] = {"", {}},
    [SYM_T][SYM_MINUS] = {"", {}},
    [SYM_T][SYM_MULTIPLY] = {"", {}},
    [SYM_T][SYM_DIVIDE] = {"", {}},
    [SYM_T][SYM_LPAREN] = {"T->FS", {&kSymbolF, &kSymbolS}, 2},
    [SYM_T][SYM_RPAREN] = {"", {}},
    [SYM_T][SYM_END] = {"", {}},

    [SYM_S][SYM_I] = {"", {}},
    [SYM_S][SYM_ADD] = {"S->ε", {}, 0},
    [SYM_S][SYM_MINUS] = {"S->ε", {}, 0},
    [SYM_S][SYM_MULTIPLY] = {"S->*FS",
                             {&kSymbolMultiply, &kSymbolF, &kSymbolS},
                             3},
    [SYM_S][SYM_DIVIDE] = {"S->/FS", {&kSymbolDivide, &kSymbolF, &kSymbolS}, 3},
    [SYM_S][SYM_LPAREN] = {"", {}},
    [SYM_S][SYM_RPAREN] = {"S->ε", {}, 0},
    [SYM_S][SYM_END] = {"S->ε", {}, 0},

    [SYM_F][SYM_I] = {"F->i", {&kSymbolI}, 1},
    [SYM_F][SYM_ADD] = {"", {}},
    [SYM_F][SYM_MINUS] = {"", {}},
    [SYM_F][SYM_MULTIPLY] = {"", {}},
    [SYM_F][SYM_DIVIDE] = {"", {}},
    [SYM_F][SYM_LPAREN] = {"F->(E)",
                           {&kSymbolLeftParen, &kSymbolE, &kSymbolRightParen},
                           3},
    [SYM_F][SYM_RPAREN] = {"", {}},
    [SYM_F][SYM_END] = {"", {}}};

#endif  // _parsing_table_h_
