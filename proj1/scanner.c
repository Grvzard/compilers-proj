
#include "scanner.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static void skipWhitespace(Scanner *sc);
static char peek(Scanner *sc);
static char peekNext(Scanner *sc);
static Token makeToken(Scanner *sc, TokenType type);
static Token errorToken(Scanner *sc, const char *message);
static char advance(Scanner *sc);
static bool match(Scanner *sc, char expected);
static bool isAtEnd(Scanner *sc);
static bool isDigit(char c);
static bool isAlpha(char c);
static Token number(Scanner *sc);
static Token identifier(Scanner *sc);
static TokenType identifierType(const char *words, int len);
static TokenType checkKeyword(const char *words, const char *expected,
                              int words_len, int expected_len, TokenType type);

void initScanner(Scanner *scanner, const char *source) {
    scanner->start = source;
    scanner->current = source;
    scanner->line = 1;
}

Token scanToken(Scanner *sc) {
    skipWhitespace(sc);
    sc->start = sc->current;
    if (isAtEnd(sc)) {
        return makeToken(sc, TOKEN_EOF);
    }

    char c = advance(sc);
    if (isAlpha(c)) return identifier(sc);
    if (isDigit(c)) return number(sc);
    switch (c) {
        case '(':
        case ')':
        case '{':
        case '}':
        case ';':
        case ',':
            return makeToken(sc, TOKEN_DELIMITERS);
        case '-':
        case '+':
        case '/':
        case '*':
            return makeToken(sc, TOKEN_OPRATORS);
        case '!':
            return match(sc, '=') ? makeToken(sc, TOKEN_OPRATORS)
                                  : errorToken(sc, "Unexpected character.");
        case '=':
        case '<':
        case '>':
            match(sc, '=');
            return makeToken(sc, TOKEN_OPRATORS);
        default:
            return errorToken(sc, "Unexpected character.");
    }
}

static Token makeToken(Scanner *sc, TokenType type) {
    Token token;
    token.type = type;
    token.start = sc->start;
    token.length = (int)(sc->current - sc->start);
    token.line = sc->line;
    return token;
}

static Token errorToken(Scanner *sc, const char *message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = sc->line;
    return token;
}

static void skipWhitespace(Scanner *sc) {
    for (;;) {
        char c = peek(sc);
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance(sc);
                break;
            case '\n':
                sc->line++;
                advance(sc);
                break;
            case '/':
                if (peekNext(sc) == '/') {
                    while (peek(sc) != '\n' && !isAtEnd(sc)) advance(sc);
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static char peek(Scanner *sc) { return *(sc->current); }

static char peekNext(Scanner *sc) {
    if (isAtEnd(sc)) {
        return '\0';
    }
    return sc->current[1];
}

static char advance(Scanner *sc) {
    sc->current++;
    return sc->current[-1];
}

static bool match(Scanner *sc, char expected) {
    if (isAtEnd(sc)) {
        return false;
    }
    if (*sc->current != expected) {
        return false;
    }
    sc->current++;
    return true;
}

static bool isAtEnd(Scanner *sc) { return *sc->current == '\0'; }

static bool isDigit(char c) { return c >= '0' && c <= '9'; }

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static Token number(Scanner *sc) {
    while (isDigit(peek(sc))) advance(sc);
    return makeToken(sc, TOKEN_NUM_UINT);
}

static Token identifier(Scanner *sc) {
    while (isAlpha(peek(sc)) || isDigit(peek(sc))) advance(sc);
    int len = (int)(sc->current - sc->start);
    return makeToken(sc, identifierType(sc->start, len));
}

static TokenType identifierType(const char *words, int len) {
    switch (words[0]) {
        case 'b':
            return checkKeyword(words, "break", len, 5, TOKEN_KEYWORDS);
        case 'c':
            return checkKeyword(words, "continue", len, 8, TOKEN_KEYWORDS);
        case 'd':
            return checkKeyword(words, "do", len, 2, TOKEN_KEYWORDS);
        case 'f':
            return checkKeyword(words, "for", len, 3, TOKEN_KEYWORDS);
        case 'i':
            if (len > 1) {
                if (words[1] == 'f') {
                    return checkKeyword(words, "if", len, 2, TOKEN_KEYWORDS);
                }
                return checkKeyword(words, "int", len, 3, TOKEN_KEYWORDS);
            }
            return TOKEN_IDENTIFIER;
        case 'r':
            return checkKeyword(words, "return", len, 6, TOKEN_KEYWORDS);
        case 'w':
            return checkKeyword(words, "while", len, 5, TOKEN_KEYWORDS);
        default:
            return TOKEN_IDENTIFIER;
    }
}

static TokenType checkKeyword(const char *words, const char *expected,
                              int words_len, int expected_len, TokenType type) {
    if (words_len == expected_len && memcmp(words, expected, words_len) == 0) {
        return type;
    }
    return TOKEN_IDENTIFIER;
}
