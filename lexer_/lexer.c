#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>


//  error handling


typedef enum {
    ERROR_NONE = 0,
    ERROR_LEXICAL,
    ERROR_UNTERMINATED_STRING,
    ERROR_UNEXPECTED_CHAR,
    ERROR_INVALID_NUMBER,
} ErrorType;

typedef struct {
    ErrorType   type;
    char*       message;
    char*       hint;         
    size_t      line;
    size_t      column;

} CPSError;

// err helper
static CPSError make_error(ErrorType type, const char* msg, const char* hint,
                           size_t line, size_t column) {
    CPSError err = {0};
    err.type    = type;
    err.message = strdup(msg);
    err.hint    = hint ? strdup(hint) : NULL;
    err.line    = line;
    err.column  = column;
    return err;
}

static void free_error(CPSError* err) {
    if (err) {
        free(err->message);
        free(err->hint);
        err->message = NULL;
        err->hint    = NULL;
    }
}


//  Token

typedef enum {
    // Keywords
    TOK_AND, TOK_APPEND, TOK_ARRAY, TOK_BOOLEAN, TOK_BYREF, TOK_BYVAL,
    TOK_CALL, TOK_CASE, TOK_OF, TOK_CHAR, TOK_CLASS, TOK_CLOSEFILE,
    TOK_CONSTANT, TOK_DATE, TOK_DECLARE, TOK_DIV, TOK_ELSE, TOK_ENDCASE,
    TOK_ENDCLASS, TOK_ENDFUNCTION, TOK_ENDIF, TOK_ENDPROCEDURE, TOK_ENDTYPE,
    TOK_ENDWHILE, TOK_EOF, TOK_FALSE, TOK_FOR, TOK_TO, TOK_FUNCTION,
    TOK_GETRECORD, TOK_IF, TOK_INHERITS, TOK_INPUT, TOK_INTEGER, TOK_MOD,
    TOK_NEW, TOK_NEXT, TOK_NOT, TOK_OPENFILE, TOK_OR, TOK_OTHERWISE,
    TOK_OUTPUT, TOK_PROCEDURE, TOK_PRIVATE, TOK_PUBLIC, TOK_PUTRECORD,
    TOK_RANDOM, TOK_READ, TOK_READFILE, TOK_REAL, TOK_REPEAT, TOK_RETURN,
    TOK_RETURNS, TOK_SEEK, TOK_STEP, TOK_STRING, TOK_SUPER, TOK_THEN,
    TOK_TRUE, TOK_TYPE, TOK_UNTIL, TOK_WHILE, TOK_WRITE, TOK_WRITEFILE,

    // literals & identifiers
    TOK_IDENTIFIER,
    TOK_NUMBER_LITERAL,
    TOK_STRING_LITERAL,
    TOK_CHAR_LITERAL,       // not implemented yet:(

    // operators & punctuation
    TOK_MINUS, TOK_ARROW, TOK_ASTERISK, TOK_FORWARD_SLASH, TOK_PLUS,
    TOK_LESS_THAN, TOK_LESS_EQUAL, TOK_NOT_EQUAL, TOK_EQUAL,
    TOK_GREATER_THAN, TOK_GREATER_EQUAL, TOK_CARET, TOK_AMPERSAND,
    TOK_COLON, TOK_LPAREN, TOK_RPAREN, TOK_LSQUARE, TOK_RSQUARE, TOK_COMMA,

    TOK_EOF_TOKEN           // special end marker
} TokenType;

typedef struct {
    char*       lexeme;       // pwned 
    TokenType   type;
    size_t      line;
    size_t      column;
    CPSError    error;        // token represents error
} Token;

static void token_free(Token* t) {
    if (t) {
        free(t->lexeme);
        free_error(&t->error);
        t->lexeme = NULL;
    }
}

//  Lexer

typedef struct {
    const char* source;
    size_t      position;
    size_t      line;
    size_t      column;
} Lexer;

static void lexer_init(Lexer* lexer, const char* source) {
    lexer->source   = source;
    lexer->position = 0;
    lexer->line     = 1;
    lexer->column   = 1;
}

static char peek(const Lexer* lexer, size_t offset) {
    size_t pos = lexer->position + offset;
    if (pos >= strlen(lexer->source)) return '\0';
    return lexer->source[pos];
}

static char advance(Lexer* lexer) {
    char c = peek(lexer, 0);
    if (c == '\0') return '\0';
    lexer->position++;
    lexer->column++;
    if (c == '\n') {
        lexer->line++;
        lexer->column = 1;
    }
    return c;
}

static void skip_whitespace(Lexer* lexer) {
    while (true) {
        char c = peek(lexer, 0);
        if (c == '\0') return;
        if (isspace((unsigned char)c)) {
            advance(lexer);
        } else {
            break;
        }
    }
}

// fd declarations
static Token handle_identifier(Lexer* lexer);
static Token handle_number_literal(Lexer* lexer);
static Token handle_string_literal(Lexer* lexer);


static Token get_next_token(Lexer* lexer) {
    skip_whitespace(lexer);

    size_t start_line   = lexer->line;
    size_t start_column = lexer->column;
    char c = peek(lexer, 0);

    if (c == '\0') {
        return (Token){ strdup(""), TOK_EOF_TOKEN, start_line, start_column, {0} };
    }

    // Single character tokens 
    switch (c) {
        case '+': advance(lexer); return (Token){ strdup("+"), TOK_PLUS, start_line, start_column, {0} };
        case '-': advance(lexer); return (Token){ strdup("-"), TOK_MINUS, start_line, start_column, {0} };
        case '*': advance(lexer); return (Token){ strdup("*"), TOK_ASTERISK, start_line, start_column, {0} };
        case '=': advance(lexer); return (Token){ strdup("="), TOK_EQUAL, start_line, start_column, {0} };
        case '^': advance(lexer); return (Token){ strdup("^"), TOK_CARET, start_line, start_column, {0} };
        case '&': advance(lexer); return (Token){ strdup("&"), TOK_AMPERSAND, start_line, start_column, {0} };
        case ':': advance(lexer); return (Token){ strdup(":"), TOK_COLON, start_line, start_column, {0} };
        case '(': advance(lexer); return (Token){ strdup("("), TOK_LPAREN, start_line, start_column, {0} };
        case ')': advance(lexer); return (Token){ strdup(")"), TOK_RPAREN, start_line, start_column, {0} };
        case ',': advance(lexer); return (Token){ strdup(","), TOK_COMMA, start_line, start_column, {0} };
        case '[': advance(lexer); return (Token){ strdup("["), TOK_LSQUARE, start_line, start_column, {0} };
        case ']': advance(lexer); return (Token){ strdup("]"), TOK_RSQUARE, start_line, start_column, {0} };

        case '/':
            if (peek(lexer, 1) == '/') {
                while (peek(lexer, 0) != '\0' && peek(lexer, 0) != '\n') {
                    advance(lexer);
                }
                if (peek(lexer, 0) == '\n') advance(lexer);
                return get_next_token(lexer); 
            } else {
                advance(lexer);
                return (Token){ strdup("/"), TOK_FORWARD_SLASH, start_line, start_column, {0} };
            }

        case '<':
            if (peek(lexer, 1) == '-') {
                advance(lexer); advance(lexer);
                return (Token){ strdup("<-"), TOK_ARROW, start_line, start_column, {0} };
            }
            if (peek(lexer, 1) == '=') {
                advance(lexer); advance(lexer);
                return (Token){ strdup("<="), TOK_LESS_EQUAL, start_line, start_column, {0} };
            }
            if (peek(lexer, 1) == '>') {
                advance(lexer); advance(lexer);
                return (Token){ strdup("<>"), TOK_NOT_EQUAL, start_line, start_column, {0} };
            }
            advance(lexer);
            return (Token){ strdup("<"), TOK_LESS_THAN, start_line, start_column, {0} };

        case '>':
            if (peek(lexer, 1) == '=') {
                advance(lexer); advance(lexer);
                return (Token){ strdup(">="), TOK_GREATER_EQUAL, start_line, start_column, {0} };
            }
            advance(lexer);
            return (Token){ strdup(">"), TOK_GREATER_THAN, start_line, start_column, {0} };

        case '"':
            return handle_string_literal(lexer);

        default:
            if (isalpha((unsigned char)c) || c == '_') {
                return handle_identifier(lexer);
            }
            if (isdigit((unsigned char)c)) {
                return handle_number_literal(lexer);
            }

            // unknown
            advance(lexer);
            CPSError err = make_error(ERROR_UNEXPECTED_CHAR,
                                      "Unexpected character",
                                      NULL, start_line, start_column);
            return (Token){ strdup(""), TOK_EOF_TOKEN, start_line, start_column, err };
    }
}

static Token handle_string_literal(Lexer* lexer) {
    size_t start_line   = lexer->line;
    size_t start_column = lexer->column;

    // consume
    advance(lexer);

    char* buf = NULL;
    size_t cap = 16;
    size_t len = 0;
    buf = malloc(cap);

    while (true) {
        char c = peek(lexer, 0);
        if (c == '\0') {
            free(buf);
            CPSError err = make_error(ERROR_UNTERMINATED_STRING,
                                      "Unterminated string literal",
                                      "Missing closing \"", start_line, start_column);
            return (Token){ NULL, TOK_STRING_LITERAL, start_line, start_column, err };
        }
        if (c == '"') {
            advance(lexer);
            break;
        }
        // append char
        if (len + 1 >= cap) {
            cap *= 2;
            buf = realloc(buf, cap);
        }
        buf[len++] = c;
        advance(lexer);
    }

    buf[len] = '\0';
    return (Token){ buf, TOK_STRING_LITERAL, start_line, start_column, {0} };
}

static Token handle_identifier(Lexer* lexer) {
    size_t start_line   = lexer->line;
    size_t start_column = lexer->column;

    char* buf = NULL;
    size_t cap = 32;
    size_t len = 0;
    buf = malloc(cap);

    while (true) {
        char c = peek(lexer, 0);
        if (isalnum((unsigned char)c) || c == '_') {
            if (len + 1 >= cap) {
                cap *= 2;
                buf = realloc(buf, cap);
            }
            buf[len++] = c;
            advance(lexer);
        } else {
            break;
        }
    }
    buf[len] = '\0';

    // kwc
    static const struct { const char* str; TokenType type; } keywords[] = {
        {"AND",         TOK_AND},         {"APPEND",      TOK_APPEND},
        {"ARRAY",       TOK_ARRAY},       {"BOOLEAN",     TOK_BOOLEAN},
        {"BYREF",       TOK_BYREF},       {"BYVAL",       TOK_BYVAL},
        {"CALL",        TOK_CALL},        {"CASE",        TOK_CASE},
        {"CHAR",        TOK_CHAR},        {"CLASS",       TOK_CLASS},
        {"CLOSEFILE",   TOK_CLOSEFILE},   {"CONSTANT",    TOK_CONSTANT},
        {"DATE",        TOK_DATE},        {"DECLARE",     TOK_DECLARE},
        {"DIV",         TOK_DIV},         {"ELSE",        TOK_ELSE},
        {"ENDCASE",     TOK_ENDCASE},     {"ENDCLASS",    TOK_ENDCLASS},
        {"ENDFUNCTION", TOK_ENDFUNCTION}, {"ENDIF",       TOK_ENDIF},
        {"ENDPROCEDURE",TOK_ENDPROCEDURE},{"ENDTYPE",     TOK_ENDTYPE},
        {"ENDWHILE",    TOK_ENDWHILE},    {"FALSE",       TOK_FALSE},
        {"FOR",         TOK_FOR},         {"FUNCTION",    TOK_FUNCTION},
        {"GETRECORD",   TOK_GETRECORD},   {"IF",          TOK_IF},
        {"INHERITS",    TOK_INHERITS},    {"INPUT",       TOK_INPUT},
        {"INTEGER",     TOK_INTEGER},     {"MOD",         TOK_MOD},
        {"NEW",         TOK_NEW},         {"NEXT",        TOK_NEXT},
        {"NOT",         TOK_NOT},         {"OF",          TOK_OF},
        {"OPENFILE",    TOK_OPENFILE},    {"OR",          TOK_OR},
        {"OTHERWISE",   TOK_OTHERWISE},   {"OUTPUT",      TOK_OUTPUT},
        {"PRINT",       TOK_OUTPUT},      // alias
        {"PRIVATE",     TOK_PRIVATE},     {"PROCEDURE",   TOK_PROCEDURE},
        {"PUBLIC",      TOK_PUBLIC},      {"PUTRECORD",   TOK_PUTRECORD},
        {"RANDOM",      TOK_RANDOM},      {"READ",        TOK_READ},
        {"READFILE",    TOK_READFILE},    {"REAL",        TOK_REAL},
        {"REPEAT",      TOK_REPEAT},      {"RETURN",      TOK_RETURN},
        {"RETURNS",     TOK_RETURNS},     {"SEEK",        TOK_SEEK},
        {"STEP",        TOK_STEP},        {"STRING",      TOK_STRING},
        {"SUPER",       TOK_SUPER},       {"THEN",        TOK_THEN},
        {"TO",          TOK_TO},          {"TRUE",        TOK_TRUE},
        {"TYPE",        TOK_TYPE},        {"UNTIL",       TOK_UNTIL},
        {"WHILE",       TOK_WHILE},       {"WRITE",       TOK_WRITE},
        {"WRITEFILE",   TOK_WRITEFILE},
        {NULL, 0}
    };

    TokenType ttype = TOK_IDENTIFIER;
    for (int i = 0; keywords[i].str; i++) {
        if (strcmp(buf, keywords[i].str) == 0) {
            ttype = keywords[i].type;
            break;
        }
    }

    return (Token){ buf, ttype, start_line, start_column, {0} };
}

static Token handle_number_literal(Lexer* lexer) {
    size_t start_line   = lexer->line;
    size_t start_column = lexer->column;

    char* buf = NULL;
    size_t cap = 16;
    size_t len = 0;
    buf = malloc(cap);

    bool has_dot = false;

    while (true) {
        char c = peek(lexer, 0);
        if (isdigit((unsigned char)c)) {
            // ok
        } else if (c == '.' && !has_dot) {
            has_dot = true;
        } else if (c == '_') {
            advance(lexer);
            continue; // ignore separator
        } else {
            break;
        }

        if (len + 1 >= cap) {
            cap *= 2;
            buf = realloc(buf, cap);
        }
        buf[len++] = c;
        advance(lexer);
    }
    buf[len] = '\0';

    if (len == 0 || (len == 1 && buf[0] == '.')) {
        free(buf);
        CPSError err = make_error(ERROR_INVALID_NUMBER,
                                  "Invalid number literal",
                                  NULL, start_line, start_column);
        return (Token){ NULL, TOK_NUMBER_LITERAL, start_line, start_column, err };
    }

    return (Token){ buf, TOK_NUMBER_LITERAL, start_line, start_column, {0} };
}


//  Public API 
typedef struct {
    Token* tokens;
    size_t count;
    size_t capacity;
    CPSError last_error;
} TokenList;

static void token_list_init(TokenList* list) {
    list->tokens = NULL;
    list->count = 0;
    list->capacity = 0;
    memset(&list->last_error, 0, sizeof(list->last_error));
}

static void token_list_push(TokenList* list, Token t) {
    if (list->count >= list->capacity) {
        list->capacity = list->capacity ? list->capacity * 2 : 64;
        list->tokens = realloc(list->tokens, list->capacity * sizeof(Token));
    }
    list->tokens[list->count++] = t;
}

static void token_list_free(TokenList* list) {
    for (size_t i = 0; i < list->count; i++) {
        token_free(&list->tokens[i]);
    }
    free(list->tokens);
    free_error(&list->last_error);
    list->tokens = NULL;
    list->count = list->capacity = 0;
}

static bool tokenize(const char* source, TokenList* out) {
    Lexer lexer;
    lexer_init(&lexer, source);
    token_list_init(out);

    while (true) {
        Token t = get_next_token(&lexer);
        if (t.error.type != ERROR_NONE) {
            out->last_error = t.error;
            token_free(&t);
            return false;
        }

        token_list_push(out, t);

        if (t.type == TOK_EOF_TOKEN) {
            break;
        }
    }

    return true;
}


static const char* token_type_name(TokenType t) {
    switch (t) {
        case TOK_AND:           return "AND";
        case TOK_APPEND:        return "APPEND";
        case TOK_ARRAY:         return "ARRAY";
        case TOK_BOOLEAN:       return "BOOLEAN";
        case TOK_BYREF:         return "BYREF";
        case TOK_BYVAL:         return "BYVAL";
        case TOK_CALL:          return "CALL";
        case TOK_CASE:          return "CASE";
        case TOK_CHAR:          return "CHAR";
        case TOK_CLASS:         return "CLASS";
        case TOK_CLOSEFILE:     return "CLOSEFILE";
        case TOK_CONSTANT:      return "CONSTANT";
        case TOK_DATE:          return "DATE";
        case TOK_DECLARE:       return "DECLARE";
        case TOK_DIV:           return "DIV";
        case TOK_ELSE:          return "ELSE";
        case TOK_ENDCASE:       return "ENDCASE";
        case TOK_ENDCLASS:      return "ENDCLASS";
        case TOK_ENDFUNCTION:   return "ENDFUNCTION";
        case TOK_ENDIF:         return "ENDIF";
        case TOK_ENDPROCEDURE:  return "ENDPROCEDURE";
        case TOK_ENDTYPE:       return "ENDTYPE";
        case TOK_ENDWHILE:      return "ENDWHILE";
        case TOK_FALSE:         return "FALSE";
        case TOK_FOR:           return "FOR";
        case TOK_FUNCTION:      return "FUNCTION";
        case TOK_GETRECORD:     return "GETRECORD";
        case TOK_IF:            return "IF";
        case TOK_INHERITS:      return "INHERITS";
        case TOK_INPUT:         return "INPUT";
        case TOK_INTEGER:       return "INTEGER";
        case TOK_MOD:           return "MOD";
        case TOK_NEW:           return "NEW";
        case TOK_NEXT:          return "NEXT";
        case TOK_NOT:           return "NOT";
        case TOK_OF:            return "OF";
        case TOK_OPENFILE:      return "OPENFILE";
        case TOK_OR:            return "OR";
        case TOK_OTHERWISE:     return "OTHERWISE";
        case TOK_OUTPUT:        return "OUTPUT";
//        case TOK_PRINT:         return "PRINT";
        case TOK_PRIVATE:       return "PRIVATE";
        case TOK_PROCEDURE:     return "PROCEDURE";
        case TOK_PUBLIC:        return "PUBLIC";
        case TOK_PUTRECORD:     return "PUTRECORD";
        case TOK_RANDOM:        return "RANDOM";
        case TOK_READ:          return "READ";
        case TOK_READFILE:      return "READFILE";
        case TOK_REAL:          return "REAL";
        case TOK_REPEAT:        return "REPEAT";
        case TOK_RETURN:        return "RETURN";
        case TOK_RETURNS:       return "RETURNS";
        case TOK_SEEK:          return "SEEK";
        case TOK_STEP:          return "STEP";
        case TOK_STRING:        return "STRING";
        case TOK_SUPER:         return "SUPER";
        case TOK_THEN:          return "THEN";
        case TOK_TO:            return "TO";
        case TOK_TRUE:          return "TRUE";
        case TOK_TYPE:          return "TYPE";
        case TOK_UNTIL:         return "UNTIL";
        case TOK_WHILE:         return "WHILE";
        case TOK_WRITE:         return "WRITE";
        case TOK_WRITEFILE:     return "WRITEFILE";

        // Literals & identifiers
        case TOK_IDENTIFIER:     return "IDENTIFIER";
        case TOK_NUMBER_LITERAL: return "NUMBER";
        case TOK_STRING_LITERAL: return "STRING";
        case TOK_CHAR_LITERAL:   return "CHAR_LITERAL";

        // Operators & punctuation
        case TOK_MINUS:          return "MINUS";
        case TOK_ARROW:          return "ARROW";
        case TOK_ASTERISK:       return "ASTERISK";
        case TOK_FORWARD_SLASH:  return "FORWARD_SLASH";
        case TOK_PLUS:           return "PLUS";
        case TOK_LESS_THAN:      return "LESS_THAN";
        case TOK_LESS_EQUAL:     return "LESS_EQUAL";
        case TOK_NOT_EQUAL:      return "NOT_EQUAL";
        case TOK_EQUAL:          return "EQUAL";
        case TOK_GREATER_THAN:   return "GREATER_THAN";
        case TOK_GREATER_EQUAL:  return "GREATER_EQUAL";
        case TOK_CARET:          return "CARET";
        case TOK_AMPERSAND:      return "AMPERSAND";
        case TOK_COLON:          return "COLON";
        case TOK_LPAREN:         return "LPAREN";
        case TOK_RPAREN:         return "RPAREN";
        case TOK_LSQUARE:        return "LSQUARE";
        case TOK_RSQUARE:        return "RSQUARE";
        case TOK_COMMA:          return "COMMA";

        case TOK_EOF_TOKEN:      return "<EOF>";

        default:                 return "UNKNOWN";
    }
}

int main(void)
{
    printf("Mini lexer REPL  (type code, press Enter)\n");
    printf("Type .exit or empty line + Enter to quit\n\n");

    char line[4096];

    while (1)
    {
        printf("> ");
        fflush(stdout);

        if (!fgets(line, sizeof(line), stdin)) {
            break; // EOF
        }

        // remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
            line[len-1] = '\0';
        }

        if (strcmp(line, ".exit") == 0 || strlen(line) == 0) {
            printf("Bye.\n");
            break;
        }

        TokenList tokens = {0};
        token_list_init(&tokens);

        if (!tokenize(line, &tokens))
        {
            fprintf(stderr, "Error %zu:%zu → %s\n",
                    tokens.last_error.line,
                    tokens.last_error.column,
                    tokens.last_error.message ? tokens.last_error.message : "?");
        }
        else
        {
            printf("\nTokens:\n");
            for (size_t i = 0; i < tokens.count; i++)
            {
                Token* t = &tokens.tokens[i];
                if (t->type == TOK_EOF_TOKEN) continue;

                printf("  %4zu:%-3zu   %-18s   '%s'\n",
                       t->line, t->column,
                       token_type_name(t->type),
                       t->lexeme ? t->lexeme : "");
            }
            printf("\n");
        }

        token_list_free(&tokens);
    }

    return 0;
}
