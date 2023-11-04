#ifndef _token_h_
#define _token_h_

typedef enum {
    TOKEN_KEYWORDS = 1,
    TOKEN_IDENTIFIER = 2,
    TOKEN_NUM_UINT = 3,
    TOKEN_OPRATORS = 4,
    TOKEN_DELIMITERS = 5,

    TOKEN_ERROR = 126,
    TOKEN_EOF = 127,
} TokenType;

typedef struct {
    TokenType type;
    const char *start;
    int length;
    int line;
} Token;

#endif
