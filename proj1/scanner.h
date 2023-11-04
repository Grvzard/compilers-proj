#ifndef _scanner_h_
#define _scanner_h_

#include "token.h"

typedef struct {
    const char *start;
    const char *current;
    int line;
} Scanner;

void initScanner(Scanner *scanner, const char *source);
Token scanToken(Scanner *scanner);

#endif
