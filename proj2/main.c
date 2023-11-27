#include <stdio.h>
#include <stdlib.h>

#include "array.h"
#include "parsing_table.h"
#include "symbol.h"

static void print_stack(int rounds, ValueArray *proc_stack,
                        ValueArray *token_queue, const char *action,
                        const char *rule) {
    printf("%2d ", rounds);
    printf(" | ");
    for (int i = 0; i < proc_stack->count; i++) {
        printf("%s", proc_stack->values[i]->repr);
    }
    printf(" | ");
    for (int i = 0; i < token_queue->count; i++) {
        printf("%s", token_queue->values[i]->repr);
    }
    printf(" | ");
    printf("%s", action);
    if (rule != NULL) {
        printf("/%s", rule);
    }
    printf("\n");
}

void simple_lex(const char *src, ValueArray *token_queue) {
    while (*src != '\0') {
        switch (*src) {
            case 'i':
                writeValueArray(token_queue, &kSymbolI);
                break;
            case '+':
                writeValueArray(token_queue, &kSymbolAdd);
                break;
            case '-':
                writeValueArray(token_queue, &kSymbolMinus);
                break;
            case '*':
                writeValueArray(token_queue, &kSymbolMultiply);
                break;
            case '/':
                writeValueArray(token_queue, &kSymbolDivide);
                break;
            case '(':
                writeValueArray(token_queue, &kSymbolLeftParen);
                break;
            case ')':
                writeValueArray(token_queue, &kSymbolRightParen);
                break;
            case '#':
                writeValueArray(token_queue, &kSymbolEnd);
                break;
        }
        src++;
    }
}

void parse(const char *src) {
    ValueArray token_queue;
    initValueArray(&token_queue);
    ValueArray proc_stack;
    initValueArray(&proc_stack);

    simple_lex(src, &token_queue);
    int err = 0;

    printf("步骤 | 分析栈 | 剩余输入符号| 动作/所用生成式 \n");
    writeValueArray(&proc_stack, &kSymbolEnd);
    writeValueArray(&proc_stack, &kSymbolE);
    int rounds = 1;
    while (!err) {
        if (proc_stack.count > 1) {
            if (token_queue.count == 0) {
                fprintf(stderr, "parse failed.\n");
                err = 1;
                break;
            }
            const Symbol *left = peekValueArray(&proc_stack);
            const Symbol *right = peekleftValueArray(&token_queue);
            if (left->type == STYPE_NONTERM) {
                // 推导
                const ParsingRule *rule = &kParsingTable[left->ns][right->ts];
                if (rule->repr[0] == '\0') {
                    fprintf(stderr, "parse failed.\n");
                    err = 1;
                    break;
                }
                print_stack(rounds, &proc_stack, &token_queue, "推导",
                            rule->repr);

                popValueArray(&proc_stack);
                for (int i = rule->rhs_len - 1; i >= 0; i--) {
                    writeValueArray(&proc_stack, rule->rhs[i]);
                }
            } else {
                // left->type == STYPE_TERM
                // 匹配
                if (left->ts != right->ts) {
                    fprintf(stderr, "parse failed.\n");
                    err = 1;
                    break;
                }
                print_stack(rounds, &proc_stack, &token_queue, "匹配", NULL);

                popValueArray(&proc_stack);
                popleftValueArray(&token_queue);
            }
        } else {
            // proc_stack.count == 1
            if (token_queue.count != 1 ||
                peekValueArray(&token_queue)->ts != SYM_END) {
                fprintf(stderr, "parse failed.\n");
                err = 1;
                break;
            }
            print_stack(rounds, &proc_stack, &token_queue, "分析成功", NULL);
            break;
        }
        rounds++;
    }

    freeValueArray(&token_queue);
    freeValueArray(&proc_stack);
}

void repl() {
    char line[1024];
    for (;;) {
        printf("> ");

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        parse(line);
    }
}

static char *readFile(const char *path) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    fseek(file, 0L, SEEK_SET);  // rewind(file);

    char *buffer = (char *)malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }
    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char *path) {
    char *source = readFile(path);
    parse(source);
    free(source);
}

int main(int argc, const char *argv[]) {
    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        printf("incorrect usage");
    }
    return 0;
}
