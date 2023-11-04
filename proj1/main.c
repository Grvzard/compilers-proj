#include <stdio.h>

#include "scanner.h"
#include "token.h"

void lex(const char *source) {
    Scanner scanner;
    Token token;
    initScanner(&scanner, source);
    for (;;) {
        token = scanToken(&scanner);
        if (token.type == TOKEN_EOF) {
            break;
        }
        printf("(%d, \"%.*s\")\n", token.type, token.length, token.start);
    }
}

void repl() {
    char line[1024];
    for (;;) {
        printf("> ");

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        lex(line);
    }
}

int main(int argc, const char *argv[]) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        // compile file
    } else {
        printf("incorrect usage");
    }
    return 0;
}
